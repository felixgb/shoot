{-# LANGUAGE QuasiQuotes #-}
-- base
import Control.Monad (when, forM_)
import Control.Exception (bracket)
import Data.Bits
import Data.Fixed (mod')
import Data.IORef
import Foreign -- includes many sub-modules
import Foreign.C.String (withCAStringLen, newCString)
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- raw-strings-qq
import Text.RawString.QQ
-- JuicyPixels
import Codec.Picture (readImage, generateImage, convertRGB8, DynamicImage(..), Image(..), PixelRGB8(..))
-- vector
import qualified Data.Vector.Storable as VS
-- linear
import Linear
-- containers
import Data.Set (Set)
import qualified Data.Set as S

winWidth = 800

winHeight = 600

winTitle = "Camera"

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: IORef (Set GLFW.Key) -> GLFW.KeyCallback
keyCallback ref window key scanCode keyState modKeys = do
    --putStrLn $ show keyState ++ " " ++ show key
    case keyState of
        GLFW.KeyState'Pressed -> modifyIORef ref (S.insert key)
        GLFW.KeyState'Released -> modifyIORef ref (S.delete  key)
        _ -> return ()
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

data MouseInfo = MouseInfo {
    lastXY :: Maybe (Double,Double),
    oldPitchYaw :: (Double, Double),
    frontVec :: V3 GLfloat
    } deriving Show

-- type CursorPosCallback = Window -> Double -> Double -> IO ()
cursorPosCallback :: IORef MouseInfo -> GLFW.CursorPosCallback
cursorPosCallback ref window xpos ypos = do
    --putStrLn $ "x: " ++ show x ++ ", y:" ++ show y
    modifyIORef ref $ \oldInfo -> let
        (lastX, lastY) = case lastXY oldInfo of
            Nothing -> (xpos,ypos)
            (Just (lastX,lastY)) -> (lastX,lastY)
        sensitivity = 0.05
        xoffset = (xpos - lastX) * sensitivity
        yoffset = (lastY - ypos) * sensitivity
        lastX' = xpos
        lastY' = ypos
        (oldPitch,oldYaw) = oldPitchYaw oldInfo
        newYaw = (oldYaw + xoffset) `mod'` 360.0
        newPitch = min (max (oldPitch + yoffset) (-89)) 89
        toRadians = realToFrac . (*(pi/180)) :: Double -> GLfloat
        pitchR = toRadians newPitch
        yawR = toRadians newYaw
        front = normalize $ V3 (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)
        in MouseInfo (Just (lastX',lastY')) (newPitch, newYaw) front

vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330 core
    layout (location = 0) in vec3 position;
    layout (location = 2) in vec2 texCoord;

    out vec2 TexCoord;

    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;

    void main()
    {
        gl_Position = projection * view * model * vec4(position, 1.0f);
        TexCoord = vec2(texCoord.x, 1.0 - texCoord.y);
    }
    |]

fragmentShaderSource :: String
fragmentShaderSource = [r|
    #version 330 core
    in vec2 TexCoord;

    out vec4 color;

    uniform sampler2D ourTexture1;
    uniform sampler2D ourTexture2;

    void main()
    {
        color = mix(texture(ourTexture1, TexCoord), texture(ourTexture2, TexCoord), 0.2);
    }
    |]

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: GLenum -> String -> IO (Either String GLuint)
loadShader shaderType source = do
    -- new shader object
    shaderID <- glCreateShader shaderType
    -- assign the source to the shader object
    withCAStringLen source $ \(strP, strLen) ->
        withArray [strP] $ \linesPtrsPtr ->
            withArray [fromIntegral strLen] $ \lengthsPtr ->
                glShaderSource shaderID 1 linesPtrsPtr lengthsPtr
    -- compile and check success
    glCompileShader shaderID
    success <- alloca $ \successP -> do
        glGetShaderiv shaderID GL_COMPILE_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right shaderID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetShaderInfoLog shaderID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the shader object and return the log
            glDeleteShader shaderID
            let prefix = case shaderType of
                    GL_VERTEX_SHADER -> "Vertex"
                    GL_GEOMETRY_SHADER -> "Geometry"
                    GL_FRAGMENT_SHADER -> "Fragment"
                    _ -> "Unknown Type"
            return $ Left $
                prefix ++ " Shader Error:" ++
                    (map (toEnum.fromEnum) logBytes)

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: GLuint -> GLuint -> IO (Either String GLuint)
linkProgram vertexID fragmentID = do
    programID <- glCreateProgram
    glAttachShader programID vertexID
    glAttachShader programID fragmentID
    glLinkProgram programID
    success <- alloca $ \successP -> do
        glGetProgramiv programID GL_LINK_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right programID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetProgramiv programID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetProgramInfoLog programID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the program object and return the log
            glDeleteProgram programID
            return $ Left $ "Program Link Error: " ++
                (map (toEnum.fromEnum) logBytes)

-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: String -> String -> IO (Either String GLuint)
programFromSources vertexSource fragmentSource = do
    eitherVertShader <- loadShader GL_VERTEX_SHADER vertexSource
    case eitherVertShader of
        Left e -> return $ Left e
        Right vertShader -> do
            eitherFragShader <- loadShader GL_FRAGMENT_SHADER fragmentSource
            case eitherFragShader of
                Left e -> do
                    glDeleteShader vertShader
                    return $ Left e
                Right fragShader -> do
                    eitherProgram <- linkProgram vertShader fragShader
                    glDeleteShader vertShader
                    glDeleteShader fragShader
                    return $ eitherProgram

verticies :: [GLfloat]
verticies = [
    -0.5, -0.5, -0.5,  0.0, 0.0,
     0.5, -0.5, -0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,

    -0.5, -0.5,  0.5,  0.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,

    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,

     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,

    -0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  1.0, 1.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,

    -0.5,  0.5, -0.5,  0.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0
    ]

cubes :: [V3 GLfloat]
cubes = [
    V3 0 0 0,
    V3 2 5 (-15),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2) (-12.3),
    V3 2.4 (-0.4) (-3.5),
    V3 (-1.7) 3 (-7.5),
    V3 1.3 (-2) (-2.5),
    V3 1.5 2 (-2.5),
    V3 1.5 0.2 (-1.5),
    V3 (-1.3) 1 (-1.5)
    ]

data Camera = Camera {
    cameraPos :: V3 GLfloat,
    cameraFront :: V3 GLfloat,
    cameraUp :: V3 GLfloat
    } deriving Show

-- uses normalized movement
updateCamera :: Set GLFW.Key -> GLfloat -> Camera -> Camera
updateCamera keySet speed cam@(Camera pos front up) = let
    moveVector = S.foldr (\key vec -> case key of
            GLFW.Key'W -> vec ^+^ front
            GLFW.Key'S -> vec ^-^ front
            GLFW.Key'A -> vec ^-^ normalize (cross front up)
            GLFW.Key'D -> vec ^+^ normalize (cross front up)
            _ -> vec
            ) (V3 0 0 0) keySet
    in cam {cameraPos = pos ^+^ (speed *^ normalize moveVector)}

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos front up) = lookAt pos (pos ^+^ front) up

main :: IO ()
main = bracketGLFW $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- enable keys
            keyRef <- newIORef S.empty
            GLFW.setKeyCallback window (Just $ keyCallback keyRef)

            -- configure the mouse
            mouseRef <- newIORef $ MouseInfo Nothing (0,(-90)) (V3 0 0 (-1))
            GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
            GLFW.setCursorPosCallback window (Just $ cursorPosCallback mouseRef)

            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            -- enable depth testing in our display
            glEnable GL_DEPTH_TEST

            -- ready and use our program
            eErrP <- programFromSources vertexShaderSource fragmentShaderSource
            shaderProgram <- case eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p
            glUseProgram shaderProgram

            -- ready our texture0
            texture0P <- malloc
            glGenTextures 1 texture0P
            texture0 <- peek texture0P
            glBindTexture GL_TEXTURE_2D texture0
            -- wrapping and filtering params would go here.
            eErrDI0 <- readImage "texture-demo.jpg"
            dyImage0 <- case eErrDI0 of
                Left e -> do
                    putStrLn e
                    return $ ImageRGB8 $ generateImage (\x y ->
                        let x' = fromIntegral x in PixelRGB8 x' x' x') 800 600
                Right di -> return di
            let ipixelrgb80 = convertRGB8 dyImage0
                iWidth0 = fromIntegral $ imageWidth ipixelrgb80
                iHeight0 = fromIntegral $ imageHeight ipixelrgb80
                iData0 = imageData ipixelrgb80
            VS.unsafeWith iData0 $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGB iWidth0 iHeight0 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glBindTexture GL_TEXTURE_2D 0

            -- ready our texture1
            texture1P <- malloc
            glGenTextures 1 texture1P
            texture1 <- peek texture1P
            glBindTexture GL_TEXTURE_2D texture1
            -- wrapping and filtering params would go here.
            eErrDI1 <- readImage "texture-demo2.jpg"
            dyImage1 <- case eErrDI1 of
                Left e -> do
                    putStrLn e
                    return $ ImageRGB8 $ generateImage (\x y ->
                        let x' = fromIntegral x in PixelRGB8 x' x' x') 800 600
                Right di -> return di
            let ipixelrgb81 = convertRGB8 dyImage1
                iWidth1 = fromIntegral $ imageWidth ipixelrgb81
                iHeight1 = fromIntegral $ imageHeight ipixelrgb81
                iData1 = imageData ipixelrgb81
            VS.unsafeWith iData1 $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGB iWidth0 iHeight0 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glBindTexture GL_TEXTURE_2D 0

            -- setup our verticies
            let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
            verticesP <- newArray verticies

            -- setup the indexes
            let indices = [  -- Note that we start from 0!
                    0, 1, 3, -- First Triangle
                    1, 2, 3  -- Second Triangle
                    ] :: [GLuint]
            let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
            indicesP <- newArray indices

            -- setup a vertex array object
            vaoP <- malloc
            glGenVertexArrays 1 vaoP
            vao <- peek vaoP
            glBindVertexArray vao

            -- setup a vertex buffer object and send it data
            vboP <- malloc
            glGenBuffers 1 vboP
            vbo <- peek vboP
            glBindBuffer GL_ARRAY_BUFFER vbo
            glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

            -- setup an element buffer object and send it data
            eboP <- malloc
            glGenBuffers 1 eboP
            ebo <- peek eboP
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
            glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

            -- assign the attribute pointer information
            let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            -- position attribute
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (5*floatSize) nullPtr
            glEnableVertexAttribArray 0
            -- texture information
            let threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)
            glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE (5*floatSize) threeFloatOffset
            glEnableVertexAttribArray 2

            -- unbind our vertex array object to prevent accidental changes in
            -- between our draw calls.
            glBindVertexArray 0

            -- Uncomment this line for "wireframe mode"
            -- glPolygonMode GL_FRONT_AND_BACK GL_LINE

            -- the names of our uniforms
            ourColor <- newCString "ourColor"
            ourTexture0 <- newCString "ourTexture0"
            ourTexture1 <- newCString "ourTexture1"
            model <- newCString "model"
            view <- newCString "view"
            projection <- newCString "projection"

            -- the pointers for our uniforms
            modelP <- malloc
            viewP <- malloc
            projP <- malloc

            -- enter our main loop
            let loop lastFrame oldCamera = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- check time
                        timeValue <- maybe 0 realToFrac <$> GLFW.getTime
                        let deltaTime = timeValue - lastFrame
                        let cameraSpeed = 5 * deltaTime
                        -- read and use keys
                        keysDown <- readIORef keyRef
                        let cameraTemp = updateCamera keysDown cameraSpeed oldCamera
                        -- read and use mouse
                        mouseInfo <- readIORef mouseRef
                        let camera = cameraTemp{cameraFront = frontVec mouseInfo}
                        -- clear the screen
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
                        -- bind textures using texture units
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D texture0
                        our0Loc <- glGetUniformLocation shaderProgram ourTexture0
                        glUniform1i our0Loc 0
                        glActiveTexture GL_TEXTURE1
                        glBindTexture GL_TEXTURE_2D texture1
                        our1Loc <- glGetUniformLocation shaderProgram ourTexture1
                        glUniform1i our1Loc 1
                        -- assign our other uniforms
                        modelLoc <- glGetUniformLocation shaderProgram model
                        viewLoc <- glGetUniformLocation shaderProgram view
                        projectionLoc <- glGetUniformLocation shaderProgram projection
                        let viewMat = toViewMatrix camera
                        let screenWidthF = fromIntegral x :: GLfloat
                        let screenHeightF = fromIntegral y :: GLfloat
                        let projMat = perspective 45 (screenWidthF / screenHeightF) 0.1 100.0
                        poke viewP (transpose viewMat)
                        poke projP (transpose projMat)
                        glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr viewP)
                        glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projP)
                        -- draw our cubes
                        glBindVertexArray vao
                        forM_ (zip cubes [0..]) $ \(cube,i) -> do
                            -- spin spin!
                            let angle = 20*(fromIntegral i) + if i `mod` 3 == 0 then timeValue else 0
                            let modelMat = mkTransformation (axisAngle (V3 (1::GLfloat) 0.3 0.5) angle) cube
                            poke modelP (transpose modelMat)
                            glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                            glDrawArrays GL_TRIANGLES 0 36
                        glBindVertexArray 0
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop timeValue camera
            loop 0.0 (Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0))
--

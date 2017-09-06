# shoot

stuff to do:

## collision
- (DONE) fix bb to use Vector
- (NOT NEEDED) set of entities instead of list
- (DONE) collision class between two entities
- health for entities?
- terrain hight map for collisions

## shoot
- limit ROF

## movement
- smooth accelerate / decelerate 
- roll on strafe

## refactor
- entities instance `Entity` class
- projectile, terrain, teapot instance `Renderable` class
- import Collision into Entity or the other way around?

## other
- more models...?
- learn blender
- use geometry shader to output face normal to fragment shader (low poly?)

## explode objects on collision

```
for each face:
    get verts in face
    increase pos in relation to model origin (by random speed)
    rot random dir
```

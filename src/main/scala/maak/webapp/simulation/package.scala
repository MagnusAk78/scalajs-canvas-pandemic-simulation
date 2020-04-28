package maak.webapp

import maak.model.physics2D.shapes.collisions.MovableCircle2D
import maak.model.physics2D.shapes.{BoundaryBox}

import scala.annotation.tailrec

package object simulation {
  private def updateEntityCollisions(fixedEntities: List[FixedEntity], movableEntities: List[MovableEntity]):Unit = {
    @tailrec
    def updateEntityCollisionsInternal(fixedEntities: List[FixedEntity], movableEntities: List[MovableEntity],
                                      checkedMovableEntities: List[MovableEntity]):Unit = {
      movableEntities match {
        case firstMa :: restMas => {
          fixedEntities.foreach { fa: FixedEntity =>
            MovableCircle2D.checkCollision(firstMa.body, fa.body) match {
              case Some(mc) => {
                firstMa.body = mc
                if(firstMa.infected && !fa.infected) {
                  fa.infect()
                } else if(!firstMa.infected && fa.infected) {
                  firstMa.infect()
                }
              }
              case None => // No collision
            }
          }

          restMas.foreach { otherMa: MovableEntity =>
            MovableCircle2D.checkCollision(firstMa.body, otherMa.body) match {
              case Some((mc1, mc2)) => {
                firstMa.body = mc1
                otherMa.body = mc2
                if(firstMa.infected && !otherMa.infected) {
                  otherMa.infect()
                } else if(!firstMa.infected && otherMa.infected) {
                  firstMa.infect()
                }
              }
              case None => // No collision
            }
          }
          updateEntityCollisionsInternal(fixedEntities, restMas, firstMa :: checkedMovableEntities)
        }
        case Nil => //Done
      }
    }

    updateEntityCollisionsInternal(fixedEntities, movableEntities, List())
  }

  // Update
  def updateAllEntities(passedTime: Double, outerBoundary: BoundaryBox, fixedEntities: List[FixedEntity],
                      movableEntities: List[MovableEntity]): Unit = {

    // Move all movableEntities and update collisions with boundary
    movableEntities.foreach{ ma: MovableEntity => {
      ma.body = ma.body.move(passedTime)
      ma.body = MovableCircle2D.checkCollision(ma.body, outerBoundary).getOrElse(ma.body)
    }}

    // Update collisions with each other
    updateEntityCollisions(fixedEntities, movableEntities)

    // Update all entities to potentially change values based on time
    fixedEntities.foreach(_.update())
    movableEntities.foreach(_.update())
  }
}

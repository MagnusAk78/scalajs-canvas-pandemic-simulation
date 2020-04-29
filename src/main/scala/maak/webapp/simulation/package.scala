package maak.webapp

import maak.model.physics2D.shapes.collisions.MovableCircle2D
import maak.model.physics2D.shapes.BoundaryBox

import scala.annotation.tailrec

package object simulation {

  // Updates all entities (move, collisions boundary/each other, and infections/immune states)
  def updateAllEntities(passedTime: Double, outerBoundary: BoundaryBox, fixedEntities: List[FixedEntity],
                        movableEntities: List[MovableEntity]): Unit = {

    // Move all movableEntities and update collisions with boundary
    movableEntities foreach(moveAndCheckBoundaryCollision(passedTime, outerBoundary, _))
    // Update all fixed entities for the time based values also
    fixedEntities.foreach(_.update())

    // Update for collisions with each other (new movements and infections)
    updateAllEntitiesForCollisions(fixedEntities, movableEntities)
  }

  private def moveAndCheckBoundaryCollision(passedTime: Double, outerBoundary: BoundaryBox,
                                            movableEntity: MovableEntity): Unit = {
    movableEntity.body = movableEntity.body.move(passedTime)
    MovableCircle2D.checkCollision(movableEntity.body, outerBoundary) match {
      case Some(newBody) => movableEntity.body = newBody
      case None => //No collision, do nothing
    }
    //While iterating, also update the time based values for the movable entity
    movableEntity.update()
  }

  private def updateAllEntitiesForCollisions(fixedEntities: List[FixedEntity],
                                             movableEntities: List[MovableEntity]): Unit = {
    @tailrec
    def updateAllEntitiesForCollisionsInternal(fixedEntities: List[FixedEntity],
                                               movableEntities: List[MovableEntity]): Unit = {
      movableEntities match {
        case firstMovableEntity :: movableEntitiesTail => {
          //Check collision between the first movable entity and all fixed entities
          fixedEntities.foreach { fixedEntity: FixedEntity =>
            MovableCircle2D.checkCollision(firstMovableEntity.body, fixedEntity.body) match {
              case Some(newBody) => {
                firstMovableEntity.body = newBody
                updateInfection(firstMovableEntity, fixedEntity)
              }
              case None => // No collision
            }
          }

          //Check collision between the first movable entity and all the other movable entities
          movableEntitiesTail.foreach { otherMovableEntity: MovableEntity =>
            MovableCircle2D.checkCollision(firstMovableEntity.body, otherMovableEntity.body) match {
              case Some((newBodyFirst, newBodyOther)) => {
                firstMovableEntity.body = newBodyFirst
                otherMovableEntity.body = newBodyOther
                updateInfection(firstMovableEntity, otherMovableEntity)
              }
              case None => // No collision
            }
          }

          //Continue the loop without firstMovableEntity
          updateAllEntitiesForCollisionsInternal(fixedEntities, movableEntitiesTail)
        }
        case Nil => //Done
      }
    }

    updateAllEntitiesForCollisionsInternal(fixedEntities, movableEntities)
  }

  private def updateInfection(entity1: Entity, entity2: Entity): Unit = entity1.state match {
    case Infected => entity2.infect()
    case Normal => {
      entity2.state match {
        case Infected => entity1.infect()
        case _ => //None infected, do nothing
      }
    }
    case Immune => //Do nothing
  }
}

package maak.webapp

import maak.model.physics2D.shapes.collisions.MovableCircle2D
import maak.model.physics2D.shapes.{Circle2D, CircleShape, Rectangle2D}

import scala.annotation.tailrec

package object simulation {

  private def updateAgentsAfterCollision(agent1: Agent, body1: CircleShape, agent2: Agent,
                                         body2: Option[CircleShape]) {
    agent1.body = body1
    agent2.body = body2.getOrElse(agent2.body)
    (agent1.infected, agent2.infected) match {
      case (true, false) => agent2.infect()
      case (false, true) => agent1.infect()
      case _ =>
    }
  }

  private def checkAndUpdateTwoAgents(agent1: Agent, agent2: Agent): Unit = agent1.body match {
    case mc1: MovableCircle2D => agent2.body match {
      case mc2: MovableCircle2D => MovableCircle2D.checkCollision(mc1, mc2) match {
        case Some((newMc1, newMc2)) => updateAgentsAfterCollision(agent1, newMc1, agent2, Some(newMc2))
        case None => // No collision, do nothing
      }
      case c2: Circle2D => MovableCircle2D.checkCollision(mc1, c2) match {
          case Some(newMc) => updateAgentsAfterCollision(agent1, newMc, agent2, None)
          case None => // No collision, do nothing
        }
      case _ => // Unhandled CircleShape
    }
    case c1: Circle2D => agent2.body match {
      case mc2: MovableCircle2D => MovableCircle2D.checkCollision(mc2, c1) match {
        case Some(newMc) => updateAgentsAfterCollision(agent2, newMc, agent1, None)
        case None => // No collision, do nothing
      }
      case c2: Circle2D => // Two non moving agents, do nothing
      case _ => // Unhandled CircleShape
    }
    case _ => // Unhandled CircleShape
  }

  def updateAgentCollisions(agents: List[Agent]) {
    @tailrec
    def updateAgentCollisionsInternal(current: Agent, checked: List[Agent], toCheck: List[Agent]) {
      toCheck match {
        case otherAgent :: toCheckTail => {
          checkAndUpdateTwoAgents(current, otherAgent)
          updateAgentCollisionsInternal(current, checked ::: List(otherAgent), toCheckTail)
        }
        case Nil => checked match {
          case nextCurrent :: nextToCheck :: restToCheck =>
            updateAgentCollisionsInternal(nextCurrent, List(), nextToCheck :: restToCheck)
          case _ => // Done
        }
      }
    }

    agents match {
      case agent :: rest => updateAgentCollisionsInternal(agent, List(), rest)
      case Nil => // Do nothing
    }
  }

  // Update
  def updateAllAgents(passedTime: Double, outerBoundary: Rectangle2D, agents: List[Agent]): Unit = {

    // Move all agents and update collisions with boundary
    agents.foreach((i: Agent) => i.body match {
      case mc: MovableCircle2D => {
        i.body = mc.move(passedTime)
        MovableCircle2D.checkCollision(mc, outerBoundary) match {
          case Some(newMc) => i.body = newMc
          case None => //
        }
      }
      case _ =>
    })

    // Update collisions with each other
    updateAgentCollisions(agents)

    // Update all agents to potentially change values based on time
    agents.foreach((i: Agent) => i.update())
  }
}

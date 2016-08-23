package experimentation

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}
import services.Counter
import yugioh._
import yugioh.action.DefaultActionModuleComponent
import yugioh.events.DefaultEventsModuleComponent


/**
  * This controller demonstrates how to use dependency injection to
  * bind a component into a controller class. The class creates an
  * `Action` that shows an incrementing count to users. The [[Counter]]
  * object is injected by the Guice dependency injection system.
  */
@Singleton
class ExperimentationController @Inject() extends Controller {

  /**
    * Create an action that responds with the [[Counter]]'s current
    * count. The result is plain text. This `Action` is mapped to
    * `GET /count` requests by an entry in the `routes` config file.
    */
  def waitingFor = Action {
    Ok(Main.player1.waitingFor)
  }

  def choose(choice: String) = Action {
    Main.player1.queue.put(choice); Ok(s"received $choice")
  }

  def viewQueue = Action {
    Ok(s"Queue: ${Main.player1.queue}")
  }

  def viewField = Action {
    Ok(s"Field:\n\n${Main.player1.fieldStr}")
  }
}

object Main extends DefaultPlayGameComponent
  with DefaultEventsModuleComponent
  with DefaultBattlePhaseModuleComponent
  with DefaultFieldModuleComponent
  with DefaultPhaseModuleComponent
  with DefaultActionModuleComponent {

  val player1 = new ThroughServerPlayer("Human")(eventsModule, fieldModule)
  val player2 = new PassivePlayer()(fieldModule)

  override val Players = (player1, player2)

  def main(): Unit = {
    try {
      playGame.mainLoop()
    } catch {
      case gameLoss: GameLoss =>
        println(s"${gameLoss.loser} has lost! " + gameLoss)
    }
  }

  val thread = new Thread(new Runnable {
    def run() {
      main()
    }
  })
  thread.start()
}

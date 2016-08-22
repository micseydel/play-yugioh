package experimentation

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}
import services.Counter


/**
  * This controller demonstrates how to use dependency injection to
  * bind a component into a controller class. The class creates an
  * `Action` that shows an incrementing count to users. The [[Counter]]
  * object is injected by the Guice dependency injection system.
  */
@Singleton
class ExperimentationController @Inject() () extends Controller {

  /**
    * Create an action that responds with the [[Counter]]'s current
    * count. The result is plain text. This `Action` is mapped to
    * `GET /count` requests by an entry in the `routes` config file.
    */
  def check = Action { Ok(Experiment.hello) }

  def set(setTo: String) = Action { Experiment.queue.put(setTo); Ok("set to " + setTo) }

}

object Experiment {
  var hello = "start"

  val queue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

  val thread = new Thread(new Runnable {
    def run() {
      while (true) {
        hello = queue.take()
      }
    }
  })
  thread.start()
}

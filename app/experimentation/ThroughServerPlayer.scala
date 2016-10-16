package experimentation

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import yugioh._
import yugioh.card.monster._
import yugioh.{FieldModule, Player, TestDeck}
import yugioh.action.Action
import yugioh.card.Card
import yugioh.card.monster.Position
import yugioh.events.{EventsModule, PhaseStartEvent, TurnStartEvent}


class ThroughServerPlayer(val name: String)(implicit eventsModule: EventsModule, fieldModule: FieldModule) extends Player {
  Me =>

  private val Nothing = "not waiting for any actions"

  /**
    * What this player is waiting for its queue to be populated with.
    */
  @volatile var waitingFor = Nothing
  @volatile var fieldStr = "turn has not yet started"

  val queue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

  def getInt(waitingFor: String): Int = {
    this.waitingFor = waitingFor
    val theInt = queue.take().toInt
    this.waitingFor = Nothing
    theInt
  }

  def getBoolean(waitingFor: String): Boolean = {
    this.waitingFor = waitingFor
    val choice = queue.take().toLowerCase
    this.waitingFor = "nothing"
    collection.Set("true", "t", "yes", "y", "1").contains(choice)
  }

  override val field = fieldModule.createField

  override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub
  eventsModule.observe { event =>
    event match {
      case TurnStartEvent(turnPlayers, mutableGameState) =>
        //        println(s"\nTurn #${mutableGameState.turnCount}")
        //        println("================================")
        showField(turnPlayers, mutableGameState)
      case phaseStart: PhaseStartEvent =>
      //        println(s"Entering ${phaseStart.phase}")
      case ignore =>
    }
  }

  private def showField(implicit turnPlayers: TurnPlayers, mutableGameState: MutableGameState) = {
    // TODO LOW: this should happen after any change to the board, and should include field zone and pendulums

    val oppSpellsTrapsMonsters = Seq(turnPlayers.opponent.field.spellTrapZones, turnPlayers.opponent.field.monsterZones).map(_.map(_.map(_.toString(this)).getOrElse("Empty")).mkString(" | ")).mkString("\n")
    val mySpellsTrapsMonsters = Seq(turnPlayers.turnPlayer.field.monsterZones, turnPlayers.turnPlayer.field.spellTrapZones).map(_.map(_.map(_.toString(this)).getOrElse("Empty")).mkString(" | ")).mkString("\n")
    fieldStr =
    s"""
         |Opponent ${turnPlayers.opponent} (${turnPlayers.opponent.lifePoints})
         |Deck (${turnPlayers.opponent.deck.remaining})
         |Hand (${turnPlayers.opponent.hand.size})
         |Grave (${turnPlayers.opponent.field.graveyard.size})
         |Banished (${turnPlayers.opponent.field.banished.size})
         |Extra Deck (${turnPlayers.opponent.extraDeck.size})
         |$oppSpellsTrapsMonsters
         |                 ---
         |$mySpellsTrapsMonsters
         |Hand (${turnPlayers.turnPlayer.hand.size}): ${turnPlayers.turnPlayer.hand.map(_.name).mkString(" | ")}
         |Deck (${turnPlayers.turnPlayer.deck.remaining})
         |Grave (${turnPlayers.turnPlayer.field.graveyard.size})
         |Banished (${turnPlayers.turnPlayer.field.banished.size})
         |Extra Deck (${turnPlayers.turnPlayer.extraDeck.size})
         |Turn player ${turnPlayers.turnPlayer} (${turnPlayers.turnPlayer.lifePoints})\n
    """.stripMargin
  }

  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState) = {
    gameState match {
      case GameState(_, _, fastEffectTiming, phase, step, _) =>
        if (actions.size == 1) {
          val action = actions.head
          //          println(s"Action $action was only option ($fastEffectTiming, $phase${Option(step).map(", " + _).getOrElse("")}), taking implicitly.")
          action
        } else {
          select("Select an action:", actions)
        }
    }
  }

  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[Card] = {
    val criteria: Criteria[Card] = new Criteria[Card] {
      override def meetable(implicit gameState: GameState) = true

      override def validSelection(choices: Seq[Card])(implicit gameState: GameState): Boolean = choices.size == hand.size - Constants.HandSizeLimit

      override def availableChoices(implicit gameState: GameState): Seq[Card] = hand
    }

    selectMultiple(s"Must discard for hand size limit ($criteria):", criteria)
  }

  override def enterBattlePhase(implicit gameState: GameState) = {
    getBoolean("MP1 is ending; enter BP? (If not, will go to EP) ")
  }

  /**
    * Ask the user for a specific element of a sequence.
    */
  private def select[A](prompt: String, choices: Seq[A])(implicit gameState: GameState): A = {

    var waitingFor = prompt + s" (${gameState.fastEffectTiming}, ${gameState.phase}${Option(gameState.step).map(", " + _).getOrElse("")})\n" +
    choices.zipWithIndex.map {
        case (action, i) => s"($i) $action"
      }.mkString("\n")

    var choice = getInt(waitingFor)
    while (choice < 0 || choice >= choices.size) {
      waitingFor += s"\n\n(an invalid option $choice was used)"
      choice = getInt(waitingFor)
    }

    choices(choice)
  }

  /**
    * Get some of the values from the choices.
    */
  private def selectMultiple[A](prompt: String, criteria: Criteria[A])
  (implicit gameState: GameState): Seq[A] = {

    val choices = criteria.availableChoices
    waitingFor = prompt + "\n" + choices.map {
      case (choice, i) => s"($i) $choice"
    }.mkString("\n")


    var selection = queue.take().split(",").map(_.toInt).map(choices(_))
    while (!criteria.validSelection(selection)) {
      waitingFor += s"\n\n$selection did not meet $criteria"
      selection = queue.take().split(",").map(_.toInt).map(choices(_))
    }
    selection
  }

  /**
    * After SP, will ask via StdIn, otherwise just consents.
    */
  override def consentToEnd(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, TurnPlayers(Me, _), _, phase@(MainPhase | BattlePhase | MainPhase2 | EndPhase), step, _) if !step.isInstanceOf[DamageStepSubStep] && !step.isInstanceOf[BattleStepWithPendingAttack] =>
        getBoolean(s"End ${Option(step).getOrElse(phase)}?")
      case _ => true
    }
  }

  override def selectSummonMaterial(summonCriteria: SummonCriteria)(implicit gameState: GameState) = {
    // TODO: streamlined logic for when there is only a single possibility
    selectMultiple(s"To summon ${summonCriteria.monster}, please enter comma separated monster(s) to use ($summonCriteria).", summonCriteria)
  }


  override def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState): Monster = {
    select(s"Select target for $attacker", potentialTargets)
  }

  override def selectEffectTargets[C <: Card](criteria: Criteria[C])(implicit gameState: GameState) = {
    selectMultiple(s"Select targets for effect.", criteria)
  }

  /**
    * For a monster in the process of being special summoned, select from positions options which position to SS in.
    */
  override def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState) = {
    select(s"Select position to special summon $monster.", positions)
  }
}

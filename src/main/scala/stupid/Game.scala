package stupid

import scala.annotation.tailrec

/**
 * @author alari
 * @since 10/8/14
 */
case class Game(
                 state: State,
                 offenceId: Int = 0,
                 defenceId: Int = 1,
                 phase: Phase.Value = Phase.Attacking

                 ) {

  def log(str: String) = {
    //println(str)
  }

  def offence = State.hand(offenceId)
  def defence = State.hand(defenceId)

  def offenceHand = offence.get(state)
  def defenceHand = defence.get(state)

  implicit def trump = state.trump

  def switch() = copy(offenceId = defenceId, defenceId = offenceId)

  def playCard(card: Card, origin: State.HandLens, nextPhase: Phase.Value = phase, updateTable: (Table,Card)=>Table = _ + _) = {
    val hand = origin.get(state)

    copy(state = (for {
      _ <- origin := (hand - card)
      _ <- State.table := updateTable(state.table, card)
    } yield ()).exec(state), phase = nextPhase)
  }

  def advance(): Game = phase match {
    case Phase.Attacking =>
      offenceHand.minOption match {
        case Some(card) =>
          // now it's time for passing
          log(s"Offence = ${offence.get(state)}")
          log(s"Defence = ${defence.get(state)}")
          log(s"Attack with $card")
          playCard(card, offence, Phase.Passing)


        case None =>
          // offender cannot attack
          log("Offender cannot attack")
          copy(phase = Phase.End)
      }

    case Phase.Passing =>
      defenceHand.minOfRanks(state.table.ranks) match {
        case Some(card) if offence.get(state).size > state.table.size =>
          // Passing succeed
          log(s"Passing with $card")
          playCard(card, defence).switch()

        case _ =>
          log(s"Cannot pass, start defending")
          copy(phase = Phase.Defending)
      }

    case Phase.Defending =>
      state.table.uncovered match {
        case card :: _ =>
          defenceHand.minToCover(card) match {
            case Some(response) =>
              log(s"Defending against $card with $response")
              playCard(response, defence, Phase.Defending, _.reply(card, _))

            case None =>
              log("Cannot reply, asking to reinforce")
              copy(phase = Phase.Reinforcing)
          }

        case Nil =>
          log("No uncovered cards here, asking to reinforce")
          copy(phase = Phase.Reinforcing)
      }

    case Phase.Reinforcing =>
      log(s"!Put (${offenceHand.minOfRanks(state.table.ranks)}) of [${state.table.ranks}] from ($offenceHand) to (${state.table}) for ($defenceHand)?")
      offenceHand.minOfRanks(state.table.ranks) match {
        case Some(card) if state.table.uncovered.size < defence.get(state).size =>
          log(s"Reinforcing with $card")
          playCard(card, offence, Phase.Defending)

        case _ =>
          state.table.uncovered match {
            case Nil =>
              log(s"Defender wins the turn")
              // defender wins the turn
              copy(state.copy(table = Table()), phase = Phase.End).switch()
            case _ =>
              log(s"Offender wins the turn")
              // offender wins
              val updatedState = (for {
                 _ <- defence := (defence.get(state) + state.table)
                _ <- State.table := Table()
              } yield ()).exec(state)

              copy(state = updatedState, phase = Phase.End)

          }

      }

    case Phase.End =>
      if(offenceHand.nonEmpty && defenceHand.nonEmpty) {
        log(s"Going to attack again")
        copy(phase = Phase.Attacking)
      } else {
        log(s"One hand is empty (${offenceHand.isEmpty}/${defenceHand.isEmpty}), smb wins")
        log("")
        // one of them wins
        copy(phase = Phase.Stop)
      }

    case Phase.Stop =>
      throw new IllegalStateException("Never call next() on stopped game")
  }

  @tailrec
  final def play(): Int = phase match {
    case Phase.Stop =>
      (offence.get(state).isEmpty, defence.get(state).isEmpty) match {
        case (true, true) => 0
        case (true, false) => offenceId + 1
        case (false, true) => defenceId + 1
        case _ => throw new IllegalStateException("Phase = Stop when no player has empty hand")
      }
    case p =>
      log(s"     -> $p | ${state.table}")
      advance().play()
  }
}

object Game {
  def read(line: String)(implicit trump: Suite) = Game(State.read(line))
}

package org.bitcoins.core.script.oracle

import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.arithmetic._
import org.bitcoins.core.script.constant._

/**
  * Created by chris on 2/1/17.
  */
trait OracleInterpreter {

  def opTickerQuery(program: ScriptProgram) : ScriptProgram = {
    require(program.script.headOption == Some(OP_TICKERQUERY), "Next script operation must be OP_TICKERQUERY")
    require(program.stack.size > 3, "We require the stack to have a time and ticker on the stack for OP_TICKERQUERY")
    val time: Long = parseTimeInSeconds(program.stack(3))
    //TODO: We need to figure out a way to encode tickers
    val ticker: String = parseTicker(program.stack(2))
    val agreedPrice = parseAgreedPrice(program.stack(1))
    val predicate = parsePredicate(program.stack.head)
    //see this conversation between sipa and I why 'doubles' are a terrible idea in consensus critical code
    //this doesn't answer the question about arbitrary precision numbers though, especially ones that do not
    //hit the stack / blockchain
    //https://botbot.me/freenode/bitcoin-core-dev/2017-02-01/?tz=America/Chicago
    //TODO: Review comments above and consider converting to an integer type, but what does this mean for our oracles?
    //do they always have to return integers?
    val spotPrice: BigDecimal = queryTicker(ticker,time)

    val bool = executePredicate(predicate,agreedPrice,spotPrice)
    val result = if (bool) OP_TRUE else OP_FALSE

    ScriptProgram(program, result :: program.stack.tail.tail.tail.tail, program.script.tail)
  }

  /** Parses a ticker from the given script token */
  private def parseTicker(token: ScriptToken): String =  "GOOG"

  /** Parses a time in seconds from the given [[ScriptToken]] */
  private def parseTimeInSeconds(token: ScriptToken): Long = token match {
    case number: ScriptNumber => number.underlying
    case x @ (_: ScriptConstant | _: ScriptOperation) =>
      throw new IllegalArgumentException("OP_TICKERQUERY expects a number as the time, got: " + x)
  }

  /** Sends an http request to our oracle requesting the price of the given ticker at the given time */
  private def queryTicker(ticker: String, time: Long): BigDecimal = {
    792.44
  }

  /** Parses the [[ArithmeticPredicateOperation]] from the given scriptToken */
  private def parsePredicate(token: ScriptToken): ArithmeticPredicateOperation = {
    //arithmetic predicate operations bytes
    val bytes: Seq[Seq[Byte]] = ArithmeticPredicateOperation.operations.map(_.bytes)
    val tokenBytes = token.bytes
    val predicateIndex = bytes.indexOf(tokenBytes)
    val predicate = if (predicateIndex == -1) {
      throw new IllegalArgumentException("Invalid ArithmeticPredicateOperation for OP_TICKERQUERY")
    } else ArithmeticPredicateOperation.operations(predicateIndex)
    predicate

  }
  /** Parses the agreed price from the given [[ScriptToken]] */
  private def parseAgreedPrice(token: ScriptToken): Long = token match {
    case number: ScriptNumber => number.underlying
    case x @ (_: ScriptConstant | _ : ScriptOperation) =>
      throw new IllegalArgumentException("OP_TICKERQUERY expects a number as the agreed price, got: " + x)
  }

  /** Executes the given [[ArithmeticPredicateOperation]] between on the given agreedPrice and spotPrice */
  private def executePredicate(predicate: ArithmeticPredicateOperation, agreedPrice: Long,
                               spotPrice: BigDecimal): Boolean = predicate match {
    case OP_NUMEQUAL => spotPrice == agreedPrice
    case OP_NUMNOTEQUAL => spotPrice != agreedPrice
    case OP_GREATERTHAN => spotPrice > agreedPrice
    case OP_GREATERTHANOREQUAL => spotPrice >= agreedPrice
    case OP_LESSTHANOREQUAL => spotPrice <= agreedPrice
    case OP_LESSTHAN => spotPrice < agreedPrice
  }
}

object OracleInterpreter extends OracleInterpreter

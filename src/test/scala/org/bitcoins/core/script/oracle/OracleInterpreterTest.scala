package org.bitcoins.core.script.oracle

import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.arithmetic.{OP_GREATERTHAN, OP_GREATERTHANOREQUAL, OP_LESSTHAN, OP_LESSTHANOREQUAL}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 2/2/17.
  */
class OracleInterpreterTest extends FlatSpec with MustMatchers {

  "OracleInterpreter" must "execute an OP_TICKERQUERY and return true to the stack when the spot price is less than the agreed price" in {
    val stack: List[ScriptToken] = List(OP_LESSTHAN, ScriptNumber(800), ScriptConstant("GOOG"),ScriptNumber(1485975576))
    val script: List[ScriptToken] = List(OP_TICKERQUERY)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = OracleInterpreter.opTickerQuery(program)
    executed.stack must be (List(OP_TRUE))
    executed.script.isEmpty must be (true)
  }

  it must "execute OP_TICKERQUERY and return true to the stack when the spot price is less than or equal to the agreed price" in {
    val stack: List[ScriptToken] = List(OP_LESSTHANOREQUAL, ScriptNumber(793), ScriptConstant("GOOG"),ScriptNumber(1485975576))
    val script: List[ScriptToken] = List(OP_TICKERQUERY)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = OracleInterpreter.opTickerQuery(program)
    executed.stack must be (List(OP_TRUE))
    executed.script.isEmpty must be (true)
  }

  it must "execute OP_TICKERQUERY and return false to the stack when the spot price is NOT less than or equal to the agreed price" in {
    val stack: List[ScriptToken] = List(OP_LESSTHANOREQUAL, ScriptNumber(792), ScriptConstant("GOOG"),ScriptNumber(1485975576))
    val script: List[ScriptToken] = List(OP_TICKERQUERY)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = OracleInterpreter.opTickerQuery(program)
    executed.stack must be (List(OP_FALSE))
    executed.script.isEmpty must be (true)
  }

  it must "execute OP_TICKERQUERY and return false to the stack when the spot price is NOT greater than agreed price" in {
    val stack: List[ScriptToken] = List(OP_GREATERTHAN, ScriptNumber(800), ScriptConstant("GOOG"),ScriptNumber(1485975576))
    val script: List[ScriptToken] = List(OP_TICKERQUERY)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = OracleInterpreter.opTickerQuery(program)
    executed.stack must be (List(OP_FALSE))
    executed.script.isEmpty must be (true)
  }

  it must "execute OP_TICKERQUERY and return false to the stack when the spot price is greater than or equal agreed price" in {
    val stack: List[ScriptToken] = List(OP_GREATERTHANOREQUAL, ScriptNumber(793), ScriptConstant("GOOG"), ScriptNumber(1485975576))
    val script: List[ScriptToken] = List(OP_TICKERQUERY)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = OracleInterpreter.opTickerQuery(program)
    executed.stack must be (List(OP_FALSE))
    executed.script.isEmpty must be (true)
  }
}

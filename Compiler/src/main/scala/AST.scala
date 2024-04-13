@author Morgan Barrett

//BinOp: '*' | '/' | '%' | '+' | '-' | '<<' | '>>' | '<' | '>' | '<=' | '>=' | '==' | 
//'!=' | '^' | '&&' | '||' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '^='
sealed trait BinaryOperator 
case object multiplyOperator extends BinaryOperator
case object divideOperator extends BinaryOperator
case object moduloOperator extends BinaryOperator
case object addOperator extends BinaryOperator
case object subtractOperator extends BinaryOperator
case object leftShiftOperator extends BinaryOperator
case object rightShiftOperator extends BinaryOperator
case object lessThanOperator extends BinaryOperator
case object greaterThanOperator extends BinaryOperator
case object lessThanOrEqualToOperator extends BinaryOperator
case object greaterThanOrEqualToOperator extends BinaryOperator
case object equalToOperator extends BinaryOperator
case object notEqualToOperator extends BinaryOperator
case object exclusiveOrOperator extends BinaryOperator
case object andOperator extends BinaryOperator
case object orOperator extends BinaryOperator
case object multiplyAssignmentOperator extends BinaryOperator
case object divideAssignmentOperator extends BinaryOperator
case object moduloAssignmentOperator extends BinaryOperator
case object addAssignmentOperator extends BinaryOperator
case object subtractAssignmentOperator extends BinaryOperator
case object leftShiftAssignmentOperator extends BinaryOperator
case object leftShiftAssignmentOperator extends BinaryOperator
case object exclusiveOrAssignmentOperator extends BinaryOperator

//Ask Vicki about this in the grammar
sealed trait Expression
case class IntLiteralExpression(value: Int) extends Expression
case class StringLiteralExpression(value: String) extends Expression
case class BoolLiteralExpression(value: Boolean) extends Expression
case class BinaryOperationExpression(left: Expression, op: BinaryOperator, right: Expression) extends Expression
case class VarNameExpression(name: VarName) extends Expression
case class ActorNameExpression(name: ActorName) extends Expression
case class EventNameExpression(name: EventName) extends Expression

//Type: 'int' | 'string' | 'bool' | 'actorname' | 'statename' | 'eventname'
sealed trait Type
case object IntType extends Type
case object StringType extends Type
case object BoolType extends Type
case object ActorNameType extends Type 
case object StateNameType extends Type
case object EventNameType extends Type

case class HsmName(name: String)

sealed trait DefActor
case object actor extends Type
//DefActor: 'actor' ActorName '{' ActorItem* '}'
case class ActorName(ActorItems: Seq[ActorItems]) extends DefActor

//Stmt: IfStmt | WhileStmt | DecStmt | AssignStmt | 
//ExitStmt | ApplyStmt | SendStmt | PrintStmt | PrintlnStmt
//IfStmt: 'if' ParenExpr Block ['else' (IfStmt | Block)] 
sealed trait Statement
case class IfStmt (guard: Expresssion, ifTrue: Statement, ifFalse: Optional[Statement]) extends Statement
case object BlockStatement (items: Seq[Statement]) extends Statement
case object WhileStmt (guard: Expression, ) extends Statement
case object DecStmt () extends Statmenet
case object AssignStmt () extends Statement
case object ExitStmt () extends Statement
case object ApplyStmt () extends Statement
case object SendStmt () extends Statement
case object PrintStmt () extends Statement
case object PrintlnStmt () extends Statement

sealed trait Block
case class Block(item: Seq[Stmt]) extends Block //check Stmt vs Statement

//def is a method, val is an instance variable

@author Morgan Barrett

//BinOp: '*' | '/' | '%' | '+' | '-' | '<<' | '>>' | '<' | '>' | '<=' | '>=' | '==' | 
//'!=' | '^' | '&&' | '||' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '^='
sealed trait BinaryOperator 
case object multiplyOperator
case object divideOperator 
case object moduloOperator 
case object addOperator 
case object subtractOperator 
case object leftShiftOperator 
case object rightShiftOperator 
case object lessThanOperator 
case object greaterThanOperator 
case object lessThanOrEqualToOperator 
case object greaterThanOrEqualToOperator 
case object equalToOperator 
case object notEqualToOperator 
case object exclusiveOrOperator
case object andOperator
case object orOperator 
case object multiplyAssignmentOperator 
case object divideAssignmentOperator 
case object moduloAssignmentOperator 
case object addAssignmentOperator 
case object subtractAssignmentOperator 
case object leftShiftAssignmentOperator 
case object leftShiftAssignmentOperator 
case object exclusiveOrAssignmentOperator

//Ask Vicki about this in the grammar
sealed trait Expression
case class IntLiteralExpression(value: Int) extends Expression
case class StringLiteralExpression(value: String) extends Expression
case class BoolLiteralExpression(value: Boolean) extends Expression
case class BinaryOperationExpression(left: Expression, op: BinaryOperator, right: Expression) extends Expression

//Type: 'int' | 'string' | 'bool' | 'actorname' | 'statename' | 'eventname'
//Ask Dewey or Vicki about this - are actor/state/event names types rather than strings?
sealed trait Type
case object IntType extends Type
case object StringType extends Type
case object BoolType extends Type
case object ActorNameType extends Type
case object StateNameType extends Type
case object EventNameType extends Type
case class HsmName(name: String)
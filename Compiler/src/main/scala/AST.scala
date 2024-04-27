/** @author Morgan Barrett 
 *  @author Jorge Enriquez
 **/

/** =================  Binary Operators  =================
 * BinOp: '*' | '/' | '%' | '+' | '-' | '<<' | '>>' | '<' | '>' | '<=' | '>=' | '==' |
 * '!=' | '^' | '&&' | '||' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '^='
 * */
sealed trait BinaryOperator 
case object MultiplyOperator extends BinaryOperator
case object DivideOperator extends BinaryOperator
case object ModuloOperator extends BinaryOperator
case object AddOperator extends BinaryOperator
case object SubtractOperator extends BinaryOperator
case object LeftShiftOperator extends BinaryOperator
case object RightShiftOperator extends BinaryOperator
case object LessThanOperator extends BinaryOperator
case object GreaterThanOperator extends BinaryOperator
case object LessThanOrEqualToOperator extends BinaryOperator
case object GreaterThanOrEqualToOperator extends BinaryOperator
case object EqualToOperator extends BinaryOperator
case object NotEqualToOperator extends BinaryOperator
case object ExclusiveOrOperator extends BinaryOperator
case object AndOperator extends BinaryOperator
case object OrOperator extends BinaryOperator
case object MultiplyAssignmentOperator extends BinaryOperator
case object DivideAssignmentOperator extends BinaryOperator
case object ModuloAssignmentOperator extends BinaryOperator
case object AddAssignmentOperator extends BinaryOperator
case object SubtractAssignmentOperator extends BinaryOperator
case object LeftShiftAssignmentOperator extends BinaryOperator
case object RightShiftAssignmentOperator extends BinaryOperator
case object ExclusiveOrAssignmentOperator extends BinaryOperator


/** =================  Types & Expressions  =================
 *  Type: 'int' | 'string' | 'bool' | 'actorname' | 'statename' | 'eventname'
 *  ActorName: NAME
 *  StateName: NAME
 *  EventName: NAME
 * */
sealed trait Type
case object IntType extends Type
case object StringType extends Type
case object BoolType extends Type
case object ActorNameType extends Type 
case object StateNameType extends Type
case object EventNameType extends Type

/** Expr: ValExpr | BinOpExpr | ApplyExpr
 *  ValExpr: VarExpr | IntExpr | StrExpr | BoolExpr | ActorExpr | StateExpr | EventExpr | ParenExpr
 *  VarExpr: STRING (removed VarName)
 *  IntExpr: NUMBER 
 *  StrExpr: STRING 
 *  BoolExpr: BOOL
 *  ApplyExpr: FuncName ExprListParen
 *  BinOpExpr: ValExpr BinOp Expr
 *  ConstExpr: IntExpr | BoolExpr | StrExpr
 *  ActorExpr: 'actor' ActorName
 *  StateExpr: 'state' StateName
 *  EventExpr: 'event' EventName
 *  ParenExpr: '(' Expr ')' 
 * */
sealed trait Expression
case class VarExpression(name: String) extends Expression
case class IntLiteralExpression(value: Int) extends Expression
case class StringLiteralExpression(value: String) extends Expression
case class BoolLiteralExpression(value: Boolean) extends Expression
case class ApplyExpression(funcName: String, args: List[Expression]) extends Expression
case class BinaryOperationExpression(left: Expression, op: BinaryOperator, right: Expression) extends Expression

case class ActorNameExpression(name: String) extends Expression
case class StateNameExpression(name: String) extends Expression
case class EventNameExpression(name: String) extends Expression

case class HsmName(name: String)


/** =================  Program Structure =================
 *  Program: DefEvent* DefGlobalConst* DefFunc* DefActor+ 
 *  DefEvent: 'event' EventName '{' [Type (',' Type )*] }' ';'
 *  DefGlobalConst: 'const' Type VarName '=' ConstExpr ';'
 *  DefFunc: 'func' FuncName FormalFuncArgs ['->' Type] Block
 *  FuncName: NAME //May be redundant; implemented as STRING
 * */
case class Program(defEvents: List[DefEvent], defGlobalConsts: List[DefGlobalConst], 
                   defFuncs: List[DefFunc], defActors: List[DefActor])
case class DefEvent(eventName: String, types: List[Type]) //Must have at least one
case class DefGlobalConst(constType: Type, varName: String, constExpr: Expression)
case class DefFunc(funcName: String, args: List[(Type, String)], returnType: Option[Type], block: Block)

/** =================  Actors & their components =================
 *  DefActor: 'actor' ActorName '{' ActorItem* '}' //ActorName implemented as STRING
 *  ActorItem: DefHSM | DefActorOn | DefMember | DefMethod
 *  DefHSM:   'statemachine' '{' StateItem* '}'
 *  DefActorOn: 'on' EventMatch OnBlock
 *  DefMember: Type VarName '=' ConstExpr ';'
 *  DefMethod: 'func' FuncName FormalFuncArgs ['->' Type] Block
 * */
case class DefActor(actorName: String, items: List[ActorItem])
sealed trait ActorItem
case class DefHSM(states: List[StateItem]) extends ActorItem
case class DefActorOn(eventMatch: EventMatch, onBlock: Block) extends ActorItem
case class DefMember(memberType: Type, varName: String, constExpr: Expression) extends ActorItem
case class DefMethod(funcName: String, args: List[(Type, String)], returnType: Option[Type], block: Block) extends ActorItem

/** =================  States & transitions ================= 
 *  StateItem: DefOn | DefEntry | DefExit | DefMember | DefMethod | DefState | InitialState
 *  DefOn: 'on' EventMatch OnBody
 *  DefEntry: 'entry' '{' Block '}'
 *  DefExit: 'exit' '{' Block '}'
 *  DefMember: Type VarName '=' ConstExpr ';'                   **DEFINED in Actors and their components**
 *  DefMethod: 'func' FuncName FormalFuncArgs ['->' Type] Block **DEFINED in Actors and their components**
 *  DefState: 'state' StateName '{' StateItem* '}'
 *  InitialState: 'initial' StateName ';'
 *  
 *  */
sealed trait StateItem
case class DefState(stateName: String, items: List[StateItem]) extends StateItem
case class DefOn(eventMatch: EventMatch, onBody: OnBody) extends StateItem
case class DefEntry(block: Block) extends StateItem
case class DefExit(block: Block) extends StateItem
case class InitialState(stateName: String) extends StateItem

/** =================  Matching events & actions ================= 
 *  EventMatch: EventName '{' [VarName (',' VarName)*] '}'
 *  OnBody: GoStmt | OnBlock
 *  GoStmt: JustGoStmt | GoIfStmt
 *  JustGoStmt: 'go' StateName Block // can be shortened
 *  GoIfStmt: 'goif' ParenExpr StateName Block ['else' (GoIfStmt | ElseGoStmt)] //what do
 *  ElseGoStmt: 'go' StateName Block
 *  OnBlock: Block
 * */

case class EventMatch(eventName: String, varNames: List[String])
sealed trait OnBody
case class GoStmt(stateName: String, block: Block) extends OnBody
case class OnBlock(block: Block) extends OnBody

/** =================  Blocks and statements ================= 
 *  Block: '{' Stmt* '}'
 *  Stmt: IfStmt | WhileStmt | DecStmt | AssignStmt | ExitStmt | ApplyStmt | SendStmt | PrintStmt | PrintlnStmt
 *  IfStmt: 'if' ParenExpr Block ['else' (IfStmt | Block)] 
 *  WhileStmt: 'while' ParenExpr Block 
 *  DecStmt: Type VarName '=' Expr ';'
 *  AssignStmt: VarName '=' Expr ';'
 *  ExitStmt: 'exit' '(' NUMBER ')' ';'
 *  ApplyStmt: ApplyExpr ';'
 *  SendStmt : HSMName '!' EventName ExprListCurly ';'
 *  PrintStmt : 'print' ExprListParen ';'
 *  PrintlnStmt : 'println' ExprListParen ';'
 *  ReturnStmt: 'return' Expr ';'
 * */
case class Block(statements: List[Statement])
sealed trait Statement
case class IfStmt (guard: Expression, ifTrueBlock: Statement, ifFalseBlock: Option[Block]) extends Statement
case class WhileStmt (guard: Expression, body: Statement) extends Statement
case class DecStmt (decType: Type, varName: String, expr: Expression) extends Statement
case class AssignStmt (theVar: Var, exp: Expression) extends Statement
case class ExitStmt (num: Int) extends Statement
case object ApplyStmt extends Statement
case class SendStmt (hsm: HSMName, event: EventName ) extends Statement
case class PrintStmt () extends Statement

/** left over stuff
 *  DefHSM:   'statemachine' '{' StateItem* '}'
 *  FormalFuncArgs : '(' [Type VarName (',' Type VarName)*] ')'
 *  ExprListParen :'(' [Expr (',' Expr)*] ')'
 *  ExprListCurly :'{' [Expr (',' Expr)*] '}'
 * */


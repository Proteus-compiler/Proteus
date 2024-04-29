/** @author Morgan Barrett 
 *  @author Jorge Enriquez
 **/

//TODO: HSMName is missing from the grammar

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
 *  Type: 'int' | 'string' | 'bool' | 'actorname' | 'statename' | 'eventname' //Is actorname different from actor
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
 *  ExprListParen :'(' [Expr (',' Expr)*] ')'
 *  BinOpExpr: ValExpr BinOp Expr
 *  ConstExpr: IntExpr | BoolExpr | StrExpr 
 *  // This needs clarifying
 *  ActorExpr: 'actor' ActorName
 *  StateExpr: 'state' StateName
 *  EventExpr: 'event' EventName
 *  ParenExpr: '(' Expr ')' 
 * */
sealed trait Expression
case class VarExpression(name: String) extends Expression // this would usually take in VarName which would hold a string
case class IntLiteralExpression(value: Int) extends Expression
case class StringLiteralExpression(value: String) extends Expression
case class BoolLiteralExpression(value: Boolean) extends Expression
case class ApplyExpression(funcName: String, args: ExprListParen) extends Expression
case class ExprListParen(expressions: Seq[Expression]) extends Expression
case class BinaryOperationExpression(left: Expression, op: BinaryOperator, right: Expression) extends Expression
sealed trait ConstExpr extends Expression
case class IntConstLiteralExpression(value: Int) extends ConstExpr
case class StringConstLiteralExpression(value: String) extends ConstExpr
case class BoolConstLiteralExpression(value: Boolean) extends ConstExpr
case class ActorNameExpression(name: String) extends Expression
case class StateNameExpression(name: String) extends Expression
case class EventNameExpression(name: String) extends Expression
case class ParenExpression(innerExpression: Expression) extends Expression



/** =================  Program Structure =================
 *  Program: DefEvent* DefGlobalConst* DefFunc* DefActor+ 
 *  DefEvent: 'event' EventName '{' [Type (',' Type )*] }' ';'
 *  DefGlobalConst: 'const' Type VarName '=' ConstExpr ';'
 *  DefFunc: 'func' FuncName FormalFuncArgs ['->' Type] Block
 *  FuncName: NAME
 * */
case class Program(defEvents: Seq[DefEvent], defGlobalConsts: Seq[DefGlobalConst],
                   defFuncs: Seq[DefFunc], defActors: Seq[DefActor])
case class DefEvent(eventName: String, types: Seq[Type]) //Must have at least one
case class DefGlobalConst(constType: Type, varName: String, constExpr: Expression)
case class DefFunc(funcName: String, args: FormalFuncArgs, returnType: Option[Type], block: Block) extends ActorItem

/** =================  Actors & their components =================
 *  DefActor: 'actor' ActorName '{' ActorItem* '}' //ActorName implemented as STRING
 *  ActorItem: DefHSM | DefActorOn | DefMember | DefMethod
 *  DefHSM:   'statemachine' '{' StateItem* '}'
 *  DefActorOn: 'on' EventMatch OnBlock
 *  DefMember: Type VarName '=' ConstExpr ';'
 *  DefMethod: 'func' FuncName FormalFuncArgs ['->' Type] Block
 *  FormalFuncArgs : '(' [Type VarName (',' Type VarName)*] ')'
 * */
case class DefActor(actorName: String, items: Seq[ActorItem]) //ActorName???
sealed trait ActorItem
case class DefHSM(stateItems: Seq[StateItem]) extends ActorItem
case class DefActorOn(eventMatch: EventMatch, onBlock: Block) extends ActorItem
case class DefMember(memberType: Type, varName: String, constExpr: Expression) extends ActorItem //Should this extend ActorItem or StateItem
case class DefMethod(funcName: String, args: Seq[(Type, String)], returnType: Option[Type], block: Block) extends ActorItem //Should this extend ActorItem or StateItem
case class FormalFuncArgs(args: Seq[(Type, String)])

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
case class DefOn(eventMatch: EventMatch, onBody: OnBody) extends StateItem
case class DefEntry(block: Block) extends StateItem
case class DefExit(block: Block) extends StateItem
case class DefState(stateName: String, items: Seq[StateItem]) extends StateItem
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

case class EventMatch(eventName: String, varNames: Seq[String])
sealed trait OnBody
sealed trait GoStmt extends OnBody
case class JustGoStmt(stateName: String, block: Block) extends GoStmt
case class GoIfStmt(condition: Expression, stateName: String, thenBlock: Block, elseGo: Option[GoStmt]) extends GoStmt
case class ElseGoStmt(stateName: String, block: Block) extends GoStmt
case class OnBlock(block: Block) extends OnBody


/** =================  Blocks and statements ================= 
 *  Block: '{' Stmt* '}'
 *  Statement: IfStmt | WhileStmt | DecStmt | AssignStmt | ExitStmt | ApplyStmt | SendStmt | PrintStmt | PrintlnStmt
 *  IfStmt: 'if' ParenExpr Block ['else' (IfStmt | Block)] 
 *  WhileStmt: 'while' ParenExpr Block 
 *  DecStmt: Type VarName '=' Expr ';'
 *  AssignStmt: VarName '=' Expr ';'
 *  ExitStmt: 'exit' '(' NUMBER ')' ';'
 *  ApplyStmt: ApplyExpr ';'
 *  SendStmt : HSMName '!' EventName ExprListCurly ';'
 *  ExprListCurly :'{' [Expr (',' Expr)*] '}'
 *  PrintStmt : 'print' ExprListParen ';'
 *  PrintlnStmt : 'println' ExprListParen ';'
 *  ReturnStmt: 'return' Expr ';'
 * */
case class Block(statements: Seq[Statement])
sealed trait Statement
case class IfStmt (guard: Expression, ifTrueBlock: Statement, ifFalseBlock: Option[Block]) extends Statement
case class WhileStmt (guard: Expression, body: Statement) extends Statement
case class DecStmt (decType: Type, varName: String, expr: Expression) extends Statement
case class AssignStmt(varName: String, expr: Expression) extends Statement
case class ExitStmt (num: Int) extends Statement
case class ApplyStmt(expression: ApplyExpression) extends Statement
case class SendStmt(hsmName: String, eventName: String, expressions: ExprListCurly) extends Statement //HSMName is not defined in the Grammar
case class ExprListCurly(expressions: Seq[Expression]) extends Expression
case class PrintStmt(expressions: ExprListParen) extends Statement
case class PrintlnStmt(expressions: ExprListParen) extends Statement
case class ReturnStmt(expression: Expression) extends Statement
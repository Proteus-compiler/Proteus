package Proteus.Parser

import Proteus.Tokenizer.Token

object ParserCombinators {

  type Parser[A] = List[Token] => Option[(A, List[Token])]

  def succeed[A](value: A): Parser[A] = tokens => Some((value, tokens))

  def fail[A]: Parser[A] = _ => None

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = tokens =>
    p(tokens).map { case (a, rest) => (f(a), rest) }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = tokens =>
    p(tokens) match {
      case Some((a, rest)) => f(a)(rest)
      case None => None
    }

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = tokens =>
    p1(tokens).orElse(p2(tokens))

  def and[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = tokens =>
    for {
      (a, rest1) <- p1(tokens)
      (b, rest2) <- p2(rest1)
    } yield ((a, b), rest2)

  def optional[A](p: Parser[A]): Parser[Option[A]] = tokens =>
    p(tokens) match {
      case Some((a, rest)) => Some((Some(a), rest))
      case None => Some((None, tokens))
    }

  def many[A](p: Parser[A]): Parser[List[A]] = tokens => {
    def loop(tokens: List[Token], acc: List[A]): (List[A], List[Token]) =
      p(tokens) match {
        case Some((a, rest)) => loop(rest, acc :+ a)
        case None => (acc, tokens)
      }

    val (results, rest) = loop(tokens, Nil)
    Some((results, rest))
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = tokens =>
    p(tokens) match {
      case Some((a, rest)) =>
        many(p)(rest).map { case (tail, rest2) => (a :: tail, rest2) }
      case None => None
    }

  def sepBy[A](p: Parser[A], sep: Parser[Unit]): Parser[List[A]] = tokens =>
    sepBy1(p, sep)(tokens).orElse(Some((Nil, tokens)))

  def sepBy1[A](p: Parser[A], sep: Parser[Unit]): Parser[List[A]] = tokens =>
    for {
      (head, rest1) <- p(tokens)
      (tail, rest2) <- many(flatMap(sep)(_ => p))(rest1)
    } yield (head :: tail, rest2)
}

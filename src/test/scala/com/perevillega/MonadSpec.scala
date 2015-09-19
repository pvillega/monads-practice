package com.perevillega

import org.scalatest.{Matchers, FlatSpec}

class MonadSpec extends FlatSpec with Matchers {


  object Empty {
    def lift[A, B](f: A => B): Empty[A] => Empty[B] = (ea: Empty[A]) => ea.map(f)
  }

  class Empty[A] {
    def map[B](f: A => B): Empty[B] = new Empty[B]
    def flatMap[B](f: A => Empty[B]): Empty[B] = new Empty[B]
  }

  class One[A](a: A) {
    def map[B](f: A => B): One[B] = new One(f(a))
    def flatMap[B](f: A => One[B]): One[B] = f(a)
  }

  class Lazy[A](a: () => A) {
    def map[B](f: A => B): Lazy[B] = new Lazy[B](() => f(a()))
    def flatMap[B](f: A => Lazy[B]): Lazy[B] = f(a())
  }

//commented due to compilation errors when running tests
//  case class Logger[A](a: A, log: List[String]) {
//    def map[B](f: A => B): Logger[B] = new Logger[B](f(a), log)
//
//    def flatMap[B](f: A => Logger[B]): Logger[B] = {
//      val Logger(a, loggerA) = f(a)
//
//      new Logger(a, log ++ loggerA)
//    }
//  }

//  case class Writer[A, L](a: A, log: L)(concat: L => L => L) {
//    def map[B](f: A => B): Writer[B, L] = new Writer[B, L](f(a), log)(concat)
//
//    def flatMap[B](f: A => Writer[B, L]): Writer[B, L] = {
//      val writerB = f(a)
//
//      new Writer(writerB.a, concat(log)(writerB.log))
//    }
//  }

  object Reader {
    type Env = Map[String, String]

    val databaseUrl: Reader[Map[String, String], String] = new Reader[Map[String, String], String] {
      override def apply(environment: Map[String, String]): String = environment("databaseUrl")
    }

    val upperCaseDatabaseUrl = databaseUrl.map(_.toUpperCase)

    val databaseType: Reader[Env, String] = ??? // lookup "type=oracle|mysql"

    val oracleUrl: Reader[Env, String] = ??? // lookup oracleUrl=...
    val mySqlUrl: Reader[Env, String] = ??? // lookup mySqlUrl=...

    val anotherDatabaseUrl: Reader[Env, String] = for {
      dbType <- databaseType
      result <- if (dbType == "oracle") oracleUrl else mySqlUrl
    } yield result
  }

  trait Reader[E, A] {
    self =>

    def apply(environment: E): A

    def map[B](f: A => B): Reader[E, B] = new Reader[E, B] {
      override def apply(environment: E): B = f(self.apply(environment))
    }

    def flatMap[B](f: A => Reader[E, B]): Reader[E, B] = new Reader[E, B] {
      override def apply(environment: E): B = f(self.apply(environment)).apply(environment)
    }
  }

  object StateFunction {
    def mapFromFlatMap[S, A, B](sa: StateFunction[S, A])(f: A => B): StateFunction[S, B] = {
      sa.flatMap(a => unit(f(a)))
    }

    def unit[S, A](a: A): StateFunction[S, A] = new StateFunction[S, A] {
      override def apply(state: S): (S, A) = (state, a)
    }
  }

  trait StateFunction[S, A] {
    self =>
    def apply(state: S): (S, A)

    def map[B](f: A => B): StateFunction[S, B] = new StateFunction[S, B] {
      override def apply(state: S): (S, B) = {
        val (newState, a) = self.apply(state)

        (newState, f(a))
      }
    }

    def flatMap[B](f: A => StateFunction[S, B]): StateFunction[S, B] = new StateFunction[S, B] {
      override def apply(state: S): (S, B) = {
        val (oldState, a) = self.apply(state)
        f(a).apply(oldState)
      }
    }
  }

  object Cont {
    def unit[A, R](a: A): Cont[A, R] = new Cont[A, R] {
      override def continue(f: (A) => R): R = f(a)
    }
  }

  trait Cont[A, R] {
    self =>

    def continue(k: A => R): R

    def map[B](f: A => B): Cont[B, R] = new Cont[B, R] {
      override def continue(k: (B) => R): R = self.continue((a: A) => k(f(a)))
    }

    def flatMap[B](f: A => Cont[B, R]): Cont[B, R] = new Cont[B, R] {
      override def continue(k: (B) => R): R = self.continue((a: A) => f(a).continue(k))
    }
  }



  trait Expr[A] {
    def map[B](f: A => B): Expr[B] = this match {
      case Value(a) => Value(f(a))
      case Add(left, right) => Add(left.map(f), right.map(f))
    }

    def flatMap[B](f: A => Expr[B]): Expr[B] = this match {
      case Value(a) => f(a)
      case Add(left, right) => Add(left.flatMap(f), right.flatMap(f))
    }
  }

  case class Value[A](a: A) extends Expr[A]
  case class Add[A](left: Expr[A], right: Expr[A]) extends Expr[A]

  object Expr extends App {
    def eval(expr: Expr[Int]): Int = expr match {
      case Value(i) => i
      case Add(left, right) => eval(left) + eval(right)
    }
  }

  "foo" should "bar" in {
    val add: Expr[Int] = for {
      a <- Value(3)
      b <- Value(4)
    } yield a + b

    println(Expr.eval(add))
  }
}

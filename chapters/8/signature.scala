import cats.Traverse

case class A(hoge: String)
case class B(fuga: String)
case class C(piyo: String)

def f01(x: IO[A], f: A => B): IO[B] = {
  for {
    pureX <- x
  } yield f(pureX)
}

def f02(x: IO[IO[A]]): IO[A] = {
  x.flatten
}

def f03(x: IO[A], f: A => IO[B]): IO[B] = {
  // (for {
  //   pureX <- x
  // } yield f(pureX)).flatten
  x.flatMap(f)
}

def f04(x: A): IO[A] = IO.delay(x)

def f05(impureAction: () => A): IO[A] = IO.delay(impureAction())

def f06(x: IO[A], alternative: IO[A]): IO[A] = x.orElse(alternative)

def f07(x: List[IO[A]]): IO[List[A]] = x.sequence

// def f08(x: Option[IO[A]]): IO[Option[A]] = x.traverse(identity)

// x が Some(IO[A]) の場合、sequence は内部の IO[A] を外に移動させ、結果として IO[Option[A]] を返します。
// x が None の場合、sequence は IO.pure(None) を返します。
def f08(x: Option[IO[A]]) : IO[Option[A]] = x.sequence

def f09(x: List[A], y: List[A]) = x.appendedAll(y)

def f10(x: List[A], f: A => Boolean): List[A] = x.filter(f)

def f11(x: List[A], zero: A, f: (A, A) => A): A = {
  x.foldLeft(zero)(f)
}

def f12(x: List[List[A]]): List[A] = x.flatten

def f13(x: List[A], f: A => List[B]): List[B] = x.flatMap(f)

def f14(x: List[A], f: A => Boolean): Boolean = x.forall(f)

def f15(x: Set[A], f: A => B): Set[B] = x.map(f)

def f16(x: Set[A], f: A => Boolean): Set[A] = x.filter(f)

def f17(x: Set[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

def f18(x: Set[Set[A]]): Set[A] = x.flatten

def f19(x: Set[A], f: A => Set[B]): Set[B] = x.flatMap(f)

def f20(x: Set[A], f: A => Boolean): Boolean = x.exists(f)

def f21(x: Option[A], f: A => B): Option[B] = x.map(f)

def f22(x: Option[A], f: A => Boolean): Option[A] = x.filter(f)

def f23(x: Option[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

def f24(x: Option[Option[A]]): Option[A] = x.flatten

def f25(x: Option[A], f: A => Option[B]): Option[B] = x.flatMap(f)

def f26(x: Option[A], f: A => Boolean): Boolean = x.forall(f)

def f27(x: String): Option[Int] = x.toIntOption

def f28(x: Option[A], alternative: Option[A]): Option[A] = x.orElse(alternative)

def f29(x: Option[A], y: B): Either[B, A] = {
  // x.match {
  //   case Some(hoge) => Right(hoge)
  //   case None => Left(y)
  // }
  x.toRight(y)
}

def f30(x: Option[A], y: B): Either[A, B] = x.toLeft(y)

def f31(x: List[Option[A]]): Option[List[A]] = x.sequence

def f32(x: Either[A, B], f: B => C): Either[A, C] = {
  // x.match {
  //   case Left(a) => Left(a)
  //   case Right(b) => Right(f(b))
  // }
  x.map(f)
}

def f33(x: Either[A, B], zero: C, f: (C, B) => C): C = {
  x.foldLeft(zero)(f)
}

def f34(x: Either[A, Either[A, B]]): Either[A, B] = x.flatten

def f35(x: Either[A, B], f: B => Either[A, C]): Either[A, C] = {
  x.flatMap(f)
}

def f36(x: Either[A, B], f: B => Boolean): Boolean = {
  x.forall(f)
}

def f37(x: Either[A, B], alternative: Either[A, B]): Either[A, B] = {
  x.orElse(alternative)
}

def f38(x: Either[A, B]): Option[B] = x.toOption

def f39(x: List[Either[A, B]]): Either[A, List[B]] = {
  x.sequence
}

def f40(x: Either[A, List[B]]): List[Either[A, B]] = {
  x.sequence
}
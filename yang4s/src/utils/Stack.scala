package yang4s.utils
import alleycats.Empty

opaque type Stack[A] = List[A]

object Stack {
  def apply[A: Empty](as: A*) = List(as*)
  def empty[A: Empty]: Stack[A] = Stack()

  extension [A: Empty as m](stack: Stack[A]) {
    def push(a: A): Stack[A] = a :: stack

    def pop: (A, Stack[A]) = {
      stack match
        case head :: tail => (head, tail)
        case Nil => (m.empty, stack)
    }

    def peak: A = stack.headOption.getOrElse(m.empty)

    def withModifiedHead(mod: A => A): Stack[A] = {
      stack.pop match
        case (a, rest) => 
          rest.push(mod(a))
    }
  }
}
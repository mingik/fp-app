package state

type State[S, +A] = S => (A, S)

case class State[S, +A](run: S => (A,S))

def get[S]: State[S, S] = State(s => (s, s))

def set[S](s: S): State[S, Unit] = State(_ => ((), s))

package kits

package object eff {
  type Opt = Exc[Unit]

  type State[S] = Reader[S] with Writer[S]
}

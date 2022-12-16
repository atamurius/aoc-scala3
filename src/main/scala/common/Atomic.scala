package common

import java.util.concurrent.atomic.AtomicReference

class Atomic[T](ref: AtomicReference[T]):
  def apply(): T = ref.get()
  def := (value: T): Unit = ref.set(value)
  def update(f: T => T): T =
    var from = ref.get()
    var to = f(from)
    while !ref.compareAndSet(from, to) do
      from = ref.get()
      to = f(from)
    to

object Atomic {
  def apply[T](value: T) = new Atomic[T](new AtomicReference[T](value))
}



// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import java.util.concurrent


sealed class Future[+a] private (f: () => a) extends Eval[a] {
    private[this] val g: () => a = {
        try {
            new Future.Execute(f)
        } catch {
            case _: concurrent.RejectedExecutionException => () => Lazy(f())._eval
        }
    }
    override def _eval: a = g()
}


object Future extends Monad[Future] with ThisIsInstance with EvalOp {
    def apply[a](a: => a): Future[a] = new Future(() => a)

    // Overrides
    //
    // Monad
    private type m[+a] = Future[a]
    override def `return`[a](a: Lazy[a]): m[a] = Future { a.! }
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = Future { k(m._eval)._eval }

    // Details
    //
    import concurrent._

    private val poolSize: Int = 2 * java.lang.Runtime.getRuntime.availableProcessors

    // A task which has internal dependencies needs direct-handoffs(SynchronousQueue).
    // (scala.actors.Future doesn't support such tasks.)
    private val executor: ThreadPoolExecutor = {
        val ex = new ThreadPoolExecutor(0, poolSize, 60L, TimeUnit.SECONDS, new SynchronousQueue[Runnable])
        ex.setThreadFactory(new DaemonThreadFactory(ex.getThreadFactory))
        ex
    }

    private final class DaemonThreadFactory(underlying: ThreadFactory) extends ThreadFactory {
        override def newThread(r: Runnable) = {
            val t = underlying.newThread(r)
            t.setDaemon(true)
            t
        }
    }

    private final class Execute[+a](f: () => a) extends (() => a) {
        private[this] val g = executor.submit {
            new Callable[a] {
                override def call() = f()
            }
        }
        override def apply() = {
            try {
                g.get
            } catch {
                case e: ExecutionException => throw e.getCause
            }
        }
    }
}

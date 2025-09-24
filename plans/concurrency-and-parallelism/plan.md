# Concurrency and Parallelism Plan

## Requirements Snapshot
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Threads | [ ] | Introduce `spawn_thread` API with join handles, TLS, and panic propagation semantics. |
| Mutex/RwLock/Condvar; Rc/Arc equivalents | [ ] | Design synchronization primitives and reference-counted ownership constructs integrating with borrow checker and poisoning semantics. |
| Atomics and memory ordering | [ ] | Define atomic types, operations, and memory order semantics aligned with hardware backends; document acquire/release guarantees. |
| Channels/message passing | [ ] | Implement MPMC channels with bounded/unbounded options, backpressure, and async compatibility. |
| Futures/async tasks | [ ] | Develop task executor, wakers, structured concurrency, and language-level async constructs. |
| Thread pools & work-stealing | [ ] | Provide scalable executor with cooperative scheduling and instrumentation hooks. |
| Cancellation & timeouts | [ ] | Establish cancellation tokens, deadline futures, and cooperative cancellation surfaces. |
| Deterministic testing harness | [ ] | Deliver deterministic scheduler replay for race detection and unit tests. |

## Near-Term Tasks
- **Draft synchronization trait interfaces** that work with forthcoming borrow-checker features.
- **Prototype channel API surface** to validate ergonomics with planned async runtime.
- **Align async task design** with `plans/runtime-errors-and-panics/plan.md` for error propagation.
- **Model atomic memory orderings** with architecture experts; update docs with litmus tests.
- **Design cancellation token RFC** including propagation semantics and drop guarantees.

## Milestones
- **M1 — Baseline Concurrency (3 weeks):** threads, join handles, TLS, panic propagation, and `Arc` prototype.
- **M2 — Synchronization Suite (4 weeks):** mutex/RwLock/Condvar with poisoning rules, lock instrumentation, and atomics API stabilized.
- **M3 — Messaging Layer (3 weeks):** bounded/unbounded channels, select-style API, deterministic test harness MVP.
- **M4 — Async Executor (4 weeks):** futures runtime with structured concurrency, cancellation, and timer wheel integration.

## Dependencies
- Platform primitives from `plans/os-and-platform/plan.md` (threading, TLS, timers).
- Language support for async/futures from `plans/advanced-language-features/plan.md`.

## Pseudocode and Complexity

### Thread Spawning
```pn
spawn_thread :: (entry_fn: (T) -> none, arg: T) -> ThreadHandle => {
	stack := allocate_stack(DEFAULT_STACK_SIZE)
	thread_id := os_create_thread(entry_fn, arg, stack)
	register_tls(thread_id, ThreadLocalState::default())
	ret ThreadHandle { id: thread_id }
}

join :: (handle: ThreadHandle) -> ThreadResult => {
	result := os_join_thread(handle.id)
	if result.panic_payload.is_some() {
		rethrow(result.panic_payload?)
	}
	release_tls(handle.id)
	ret result.value
}
```
- **Time:** `spawn_thread` O(1) amortized (delegated to OS). `join` O(T) where T is time until thread completion plus panic unwinding cost.
- **Space:** O(1) per handle plus OS-managed stack per thread and TLS metadata.

### Mutex Lock/Unlock
```pn
lock :: (mutex: &Mutex) -> none => {
	loop {
		if atomic_compare_exchange(mutex.state, MutexState::Unlocked, MutexState::Locked) {
			break
		}
		park_current_thread()
	}
	mutex.owner = current_thread()
}

unlock :: (mutex: &Mutex) -> none => {
	mutex.owner = none
	mutex.state.store(MutexState::Unlocked)
	unpark_waiters(mutex)
}
```
- **Time:** Lock attempt O(1) expected; worst-case depends on contention. Unlock O(1).
- **Space:** O(1) auxiliary.

### Work-Stealing Executor
```pn
steal_loop :: (worker: &Worker) -> none => {
	loop {
		task := worker.pop_local()
		if task.is_none() {
			victim := scheduler.pick_victim(worker.id)
			task = scheduler.steal(victim)
		}
		match task {
			none => {
				if scheduler.is_shutdown() { break }
				yield_processor()
			},
			some(job) => {
				result := job.poll()
				if result == Poll::Pending {
					worker.push_local(job)
				}
			},
		}
	}
}
```
- **Time:** Expected O(1) per poll; steals introduce O(log N) contention overhead due to victim selection.
- **Space:** O(N) for worker deques plus scheduler metadata.

### MPMC Channel (bounded)
```pn
Slot :: struct {
	state: AtomicSlotState,
	value: Cell[T],
}

push :: (channel: &BoundedChannel[T], item: T) -> none => {
	loop {
		tail := channel.tail.load()
		slot := &channel.buffer[tail % channel.capacity]
		if slot.state.load() == SlotState::Full {
			wait_not_full(channel)
			continue
		}
		if slot.state.compare_exchange(SlotState::Empty, SlotState::Writing) {
			slot.value.store(item)
			slot.state.store(SlotState::Full)
			channel.tail.fetch_add(1)
			signal_not_empty(channel)
			ret
		}
	}
}

pop :: (channel: &BoundedChannel[T]) -> T => {
	loop {
		head := channel.head.load()
		slot := &channel.buffer[head % channel.capacity]
		if slot.state.load() ~= SlotState::Full {
			wait_not_empty(channel)
			continue
		}
		item := slot.value.load()
		if slot.state.compare_exchange(SlotState::Full, SlotState::Empty) {
			channel.head.fetch_add(1)
			signal_not_full(channel)
			ret item
		}
	}
}
```
- **Time:** Amortized O(1) per push/pop under low contention; depends on scheduler fairness.
- **Space:** O(C) for capacity-sized ring buffer.

### Cancellation Token Propagation
```pn
cancel_after :: (future: Future[T], deadline: Instant, token: CancellationToken) -> Future[!(T | Cancelled)] => {
	ret Future::new(move || {
		loop {
			if token.is_cancelled() || monotonic_now() >= deadline {
				future.cancel()
				ret !Cancelled
			}
			match future.poll() {
				Poll::Ready(value) => ret value,
				Poll::Pending => park_until(deadline.min(token.next_update())),
			}
		}
	})
}
```
- **Time:** O(K) for K polls until completion or cancellation event.
- **Space:** O(1) auxiliary plus token state.

### Async Executor Tick
```pn
executor_run :: (queue: &mut TaskQueue) -> none => {
	loop {
		task_opt := queue.pop()
		match task_opt {
			none => break,
			some(task) => {
				result := task.poll()
				if result == Poll::Pending {
					queue.push(task)
				}
			}
		}
	}
}
```
- **Time:** Each poll O(P) where P work performed; run loop O(N·P) for N tasks in queue.
- **Space:** O(N) for task queue entries.

## an implementation of miniKanren in OCaml (Parallel version)

[original implementation in OCaml](https://github.com/manshengyang/minikanren-ocaml)

to run tests:
```
dune build
dune test
```
main file: `/test/u.ml`
Example tasks are in `/lib/examples`
Parallel Conde implementation in `/lib/mk.ml`

### MultiCore OCaml
This project is an attempt of parallelism in the environment of Multicore OCaml. 
The [Eio Multicore OCaml library](https://github.com/ocaml-multicore/eio) was used for parallelism:
- [Domains manager](https://ocaml-multicore.github.io/eio/eio/Eio/Domain_manager/index.html). Parallel computation across multiple CPU cores.
- [Fiber](https://ocaml-multicore.github.io/eio/eio/Eio/Fiber/index.html) -- light-weight thread
- [Stream](https://ocaml-multicore.github.io/eio/eio/Eio/Stream/index.html) -- thread-safe queue
- [Atomic](https://v2.ocaml.org/api/Atomic.html) (from standard library)

### Parallel Conde


Parallelism is achieved by running the clauses of disjunction in several threads (domains) and then merging the answer streams. You can find the implementation  in `/lib/mk.ml`.

`domains_limit` -- it could be useful to adjust the number of domains. 
`finish_flag` -- is used for stopping forcing lazy computations when all the needed answers were received.
`queue` -- data structure for collecting answers from all the domains.
`fun force_streams` -- forcing lazy computations. Is used in running domains
`fun make_task_list` -- runs list of tasks in two modes: parallel and nonparallel. In the current version if the domains number exceeds the `domain_limit`, next task would be run in nonparallel mode. 
`fun merge_streams` -- gets all the answers from `queue` and merges them 



Notes:
- Previously there was a problem with correctness of answers. It was fixed by replacing global variables with thread-safe types. Now it's correct on all example tasks. 
- `finish_flag` is reset to false before calling `merge_streams queue`, because in this case we need to force lazy computations to get collected answers. This part of code is not parallel, so there should be no troubles

## Some results
Exact time results depends on your computer. 

- Potential problem when run this with limit 100 and more (execution time is much longer than on nonparallel version). 
	```
	[conde_par [
		[reverso_par q q];
		[reverso_par q q]
	]]
	```

All other cases are faster than their nonparallel versions. 
Let's consider `lst` is a defined list which we can reverse and get answer. Some examples:
- 100 `reverso lst` combined with `conde_par` (16s vs 51s) -- let's call it smart usage of `conde_par`, because in this example `conde` and `conde_par` are combined
- 100 `reverso_par lst` combined with `conde_par` (18s(or 30s) vs 51 s) -- replaced all `conde` with `conde_par`
- If we use nonparallel reverso and combine them with parallel conde, 6s vs 12s (fully non-parallel)
	```
		[conde_par [
			[reverso q q];
			[reverso q q]
		]]
	```
[todo: more examples]


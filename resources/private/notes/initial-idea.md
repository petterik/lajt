## Notes from a plane between Montreal and San Francisco

What are the essentials?

Read, mutate, parser?, send, merge, render, state?, response, routes, ?

What can we learn from Posh, Fulcro?


## Excluded from om.next
QueryParams doesn't work.
- We can have dynamic queries anyway, since the reads can be ultra powerful.

Dynamic queries doesn't work.
- Which means that we can simplify the indexing logic a lot?

Don't need merge-tree, tree->db, db->tree.


## Reads

Read results should be cachable, to return reads instantly.
- Doesn't take up any extra space thanks to immutability.
- Reads can be cached in om.next, but a pass of path-meta is needed, which destroys (free) pointer equality checks.

Changes to data should reactively update UI
- om.next doesn't have this, as it doesn't know the underlying database format.
- Equality checks can be run understand if data needs updating.

Reads should (must?) declare their query.
- Reads without query cannot have a pattern?
  - Making them work as they do in re-frame.

Can we make counts or similar queries fast?
- Counts can be done instead of pull. That's interesting.
- Could we make it possible to depend on other read's queries?
- Example
  - :query/notifications
  - :query/notification-count
  - They both specify the same entity-query but one pulls and the other counts.
- Well, ok, how about totally custom reads.
  - For the ones that don't rely on app-state data
    - They can't have a pull-pattern
    - They will be re-run everytime the component changes.
    - They will be re-run whenever we merge the key
    - They will be re-run whenever an action explicitly says they should be re-run.
  - For the ones that does very custom logic and has a pull pattern
    - They can depend on 1 or many other reads for their query data.
    - They can use the post-read function (for sort and stuff) to do whatever they want.
    - This way we've specified which data they depend on.


Seems like we want a post-query hook for reads?
- This is possibly just an intersector
- So that the user can sort and stuff after the query/pull is done.
  - This would be the value we cache.

Should be easier to declare whether a read should be remote or not?
- No: (if target {:remote true} ..)

## Mutates
Works pretty well?

Specify reads that should be forcibly re-run?
- Like the example where there's no query and no pattern.

## Parsing - Will we still have it?
What's nice about parsing, is that it's easy to run and extend a parser.
Defining "global rules" works pretty well UI programming

The parser is really the engine for reads, mutate and somewhat sends. It can change the outcome of all of them and. It's nice to have such functionality so easily extended.

Parsing should dedupe query.

## Send
Should be pluggable, but default to git-rebase.
One shouldn't have to define an action and an inverse-action.

## Merge

Multimethod for merge has been great for us. 
- Rarely used but gives a lot of power.

Should possibly also always call a function whenever a key or mutate is read.
- To be able to do "post-merge" stuff.

## Render
Defining UI components must be easier to do.
Having a map with life-cycle methods is a must.
Defining queries should be nicer.
Also possibly defining which children one has in a vector: [Product Login]
- Possibly associated with a key: [[:product Product] Login]
  - If one needs multiple or something?

## Component state
Do we care?
What does reagent do?
I think they use reactive atoms, which are sharable. Pretty cool?

## Response

I really like the way we implemented messages. 
Something similar usage wise would be nice.
Implementation could be different?

Read responses as well?
- (I.e. listen for if we've gotten a read or not).
- Has always been an anti-pattern.
- What do we want here?
- I think we want: 
  - "Have we read remotely ever?"
  - "Have I been unmounted have we read since?"

## Routes
What can we do to help with routes?
- Registering routes on a multimethod allows for code splitting
  - Which is nice to have.

Make it really easy to depend on routes, route-params or query-params.

# What really makes this possible is the tech that can reactively update UI components, quickly, given a query and a pull-pattern.

Entity was changed
- Entity attribute was added
- Entity attribute was deleted
- Entity attribute was changed.
Entity was added
Entity was deleted

How many entities are there in our database?
- Like, what if we store each entity that was returned for a read.
- Check if any entity was changed

To update reactively.
- For each new tx-data,
  - If entity is new
    - Re-run reads that includes the attribute/entity.
      - Possibly do something smarter, where we can incrementally update the read data.
  - Lookup which reads the entity belongs to.
    - Possibly incrementally update read results based on new tx-data.
      - If the entity was in the pull-pattern:
        - We could store the path of the entity-map, and apply new tx-data to the map.
      - If the entity was relevant to the query:
        - If the entity could be one of the return values
          - Pass it as input
          - else: Maybe re-run the whole query?
  - We're doing a lot of indexing.
    - Could we index the data via Datascript and have our reactive logic be datalog?

Looking through our client/parser/read.cljc it seems like what's been described would work for us.
- We mostly use (db/pull-x-with db query eid(s)).
- And it's pretty cool that this could allow us to cache ids and pull separately.

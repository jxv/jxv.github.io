# Version You an API for Great Good!

**October 20th, 2017**

### Fluid, The Programmatic IDL

[Fluid](https://github.com/jxv/fluid) is yet another API-silver-bullet on the block.
And it boasts a wacky tagline, “The Programmatic IDL.”

Yes, a programmatic API.
All queries are complete programs[1].
The requester sends a JSON[2] value to a language interpreter running on the server.
The server runs the program making multiple API calls in one fell swoop[3].
On the client side, Fluid provides and generates a typesafe, query DSL.
This is a possible benefit of IDLs.
It makes for a great pairing as you won’t be visibly stuck with JSON.
Instead, you write in your language of choice.

While that sounds very well — flashy and interesting.
It isn’t the main motivation as hinted in the title.

It’s the way Fluid handles versioning/evolution.
I believe versioning is the cornerstone feature for APIs.
It should be taken with respective importance.
_Lemme explain._

## The Versioning Problem

There are many ways to make a mess of RESTful APIs. Versioning is just one way. Rather than iterating through the problems inside ad hoc solutions, we’ll iterate through the general problems.

### Why version?

For starters, you don’t necessarily integrate with an API you own. Second, these interfaces exist in different code bases. They are mutable and easy to forget. Features get added, removed, and updated. These kinds of changes should be represented somehow.

### Where to version

The version can be placed somewhere inside the header, possibly using a registered vendor MIME type. Or inside the request payload. Or even as a query parameter. Or as a subdomain. My favorite solution is simply to prefix an endpoint’s path with `/v1/` , `/v2/`, `/v3/`, etc. Nevertheless, the overall lack of consistency with placement is nothing short of ironic.

### What to version

What exactly constitutes a major breaking change or minor change? Is it when a new endpoint is added? Old one removed? A type or resource is changed? Query parameters optionalized? Headers adjusted.

That line becomes horrifically blurry when optionalizing fields on JSON objects. It abuses the optional aspect to act as a means for both API introductions and deprecations. Nevermind you’d need to deal with the resulting edge cases from excessive optionalizing.

### How to version

Hopefully, your APIs are versioned by a reproducible process.
It could be based on features, resources, or endpoints.
Gut feelings or “it’s been long enough” aren’t reproducible.
Nevertheless, it’s still a flesh-bodied someone manually bumping the versions in the end.
People make mistakes.

## Automating the version

With a reproducible versioning process in place, it could then be automated.

Fluid does automatic semantic versioning, so we'll talk about versions in those terms.
Major is breaking.
Minor is non-breaking.
Patch is ignored because it's a server implementation detail.

### Diff for changes

By now most programmers understand the value of version control.
Seeing and storing diffs between different code commits is extremely useful.
Whether you realize it or not, every diff semantically represents a version change — even all the way down to the microscopic happenings.
It may not be a major or minor change, but it’s a change nonetheless.

The same diff’ing idea can be applied to APIs if they were represented as code.
And they can.
This is where IDLs come into play.

### APIs as an IDL Spec

If you’ve ever dealt with Thrift, ProtoBuf, or another IDL, you understand this already.

In a typical and self-contained code base, we abstract effects through some kind of interface.
It could be a class, a monad, or whatever.
An IDL enforces that abstraction into a language agnostic interface.
This interface is the spec.
With a generator, the spec creates a client side SDK and a server-side stubs.
It doesn’t have to stop there.
The spec can create documentation as well.

### Diff’ing the Specs

Consider this simple pseudo-code IDL spec.
It starts with version `0.0`.

```
function Hello(target: String) -> String
```
_<sup><sub>Figure: Spec 1</sub></sup>_

Duplicate it and add a custom greeting.

```
function Hello(greeting: String, target: String) -> String
```
_<sup><sub>Figure: Spec 2</sub></sup>_

Now you have two specs[4].

We added another argument to the function[5] `Hello`.
Because not all changes are created equal, there will be either a major or minor version difference.

Because any client that depended on the previous version of `Hello` will break, it’s a breaking change — a new major version at `1.0`.
You may argue `greeting` should have a default value as decided by the previous implementation.
It shouldn’t.
Doing that is a skewed variation of the optionalizing hack.
Those differences should remain in the server implementation and not in the spec.

Alright.
Let’s make a couple more changes.

```
function Hello(greeting: String, target: String) -> String

type Color = Red | Green | Blue

function FavoriteColor() -> Color
```
_<sup><sub>Figure: Spec 3</sub></sup>_

Because `Color` and `FavoriteColor` don’t modify or remove from the previous, the difference is a minor bump to version `1.1`. Everything is still possible as it was in `1.0`.

```
function Hello(greeting: String, target: String) -> String

type Color = Red | Green | Blue | Yellow

function FavoriteColor() -> Color
```
_<sup><sub>Figure: Spec 4</sub></sup>_

The added `Yellow` to the `Color` type is a bit more subtle.
It causes a major version bump to version `2.0`.
The reason is the previous client implementation can’t exhaustively match the output of `FavoriteColor`.


For the sake of completeness, the spec below removes the function `FavoriteColor` causing a major version bump as well.

```
function Hello(greeting: String, target: String) -> String

type Color = Red | Green | Blue | Yellow
```
_<sup><sub>Figure: Spec 5</sub></sup>_

It’s now version `3.0`.


To draw a conclusion from this exercise, I’ll extend to the following:

> _All modifications and removals of existing types and existing functions break APIs._
> _Additions of new types and new functions don’t._

## Handling requests with different versions

The example specs gives creates 4 different major versions: `0.0`, `1.1`, `2.0`, and `3.0`. 
Version `1.0` can be excluded from generation as `1.1` can cover exactly the same aspects.

### Dispatch to service

While I think versioned endpoints makes the most sense for RESTful APIs,
IDLs only really need a single endpoint.
Everything important happens through the payload anyways.
By passing the version inside the payload, the client offloaded a little work to the server.
IDL generated servers are plenty capable dispatching to the correct service version.

### Type reuse

An IDL, which is smart enough to recognize changes between types in different specs, shouldn't recreate the same types.
Especially for statically type languages, it's useful.
Function definitions can be reused if all the types involved are the same between different service versions.

## Deprecating versions

Not all of versioning is about adding new versions.
Sometimes you need to remove old ones.
Everyone rightfully does this differently.
API sunsetting needs a flexible deprecation system.

### Dropping support

How do you specify which versions to drop?
Stating a range of the supported major versions sounds like a common approach.
That should be easy then.
Obviously, it also implies anything outside the range is not supported.
You'll likely want to support the latest version, so just mentioning the oldest supported version is enough.

What about type reuse from older unsupported versions?
Those get bumped up into new oldest version.

### Manual curation

The real world is complicated.
How you choose what APIs to support long term doesn't have to follow predictable guidelines.
Whether it's support for important clients or in-house legacy tools,
an automated versioner shouldn't over impose.
There needs to be a fallback.

Overriding the version inside the spec is a way out.
It's something I don't eagerly recommend because the abuse defeats the purpose of automated versioning.
Yet, that option becomes a no-brainer when you need absolutely need it.

## Fluid’s Implementation

Fluid applies all the theories described above.
In practice, there were many, snickering devils hiding in the details.
Not suprising.
Siloed ideas and working code in one area evolved into becoming needless and overcomplicated in another area.
For these reasons, many parts of Fluid were thrown out, redesigned, and rewritten over the better part of a year.
I believe this to be a strength which you can benefit from.

### The spec

A Fluid spec is in the pragmatic JSON format.
Admittedly, it was a hurdle to shoehorn, yet what turned out is far from terrible.
But you can judge for yourself.

Here’s the first spec translation into a real Fluid spec.

```
{
  "fluid": { "major": 0, "minor": 0 },
  "types": [
    { "n": "Hello", "m": [{"target":"String"}], "o": "String" }
  ],
  "pull": {
    "name": "Example",
    "meta": "Unit",
    "error": "Unit",
    "protocol": "http",
    "host": "localhost",
    "port": 8080,
    "path": "/"
  }
}
```
_<sup><sub>Figure: Spec 1, Version 0.0</sub></sup>_

That's many lines more compared the original pseudo-code spec.
Let's them break down by keys.

#### Types

The only remotely recognizable thing is the `Hello` line. Quick comparsion to the fake spec:

```
function Hello(target: String) -> String

{ "n": "Hello", "m": [{"target":"String"}], "o": "String" }
```

The `"n"` means name and `"o"` means output.
Easy comparsion.
The `"m"` is trickier though.
It means members.
_Wait. Isn't this a function? Shouldn't it mean arguments?_
Yes and no.

It's true that `Hello` the function takes an argument.
The argument is not `target`, it's `Hello` the struct.
`Hello` the struct has the member `target`.
The full reasoning is explained later in the __What's next > Event sourcing__ section.
For now, just understand they're different.

Here's a closer equivalent in pesudo-code:
```
type HelloTheStruct = { target: String }

function HelloTheFunction(arg: HelloTheStruct) -> String
```

Types and functions exist in separate namespaces in Fluid, so the names are overloaded.

#### Fluid

The value of key `"fluid"` describes the implementation version of the  transport.
It does not describe the generated API version.
Fluid is a newborn, so it's currently `{"major": 0, "minor": 0}`.
It definitely will be needed when features are added, modified, or removed.


#### Pull

The name `"pull"` comes by comparison to push, like push notifications.
It's just RPC.
The object contains where and how to communicate with the server.

##### Address

The address is the combined make up of: `"protocol"`, `"host"`, `"port"`, and `"path"`.
To keep things simple, they're kept separated.

##### Meta

`"meta"` is akin to the HTTP usage of headers.
There aren't rules how to use it.
However, authentication information is a good fit.
This spec passes a meaningless unit type.

##### Error

HTTP status codes doesn't cover all possible error cases.
They would only cover the some amongst all IDL servers.
But they won't cover the errors related to a specific service.
That's what this `"error"` represents.
Like `"meta"` it can be any type the creator desires.

### Next spec

Here's the next spec, prepending `greeting` to the member list.

```
{
  "fluid": { "major": 0, "minor": 0 },
  "types": [
    {
      "n": "Hello",
      "m": [{"greeting":"String"}, {"target":"String"}],
      "o": "String"
    }
  ],
  "pull": {
    "name": "Example",
    "meta": "Unit",
    "error": "Unit",
    "protocol": "http",
    "host": "localhost",
    "port": 8080,
    "path": "/"
  }
}
```
_<sup><sub>Figure: Spec 2, Version 1.0</sub></sup>_

Order is important for members.
Not all languages treat members as elements in a dictionary or a (hash)map.
They require an order for each of the members.
There's not a reasonable alternative unfortunately.
So it's better to be consistent about ordering.

Recall that the automated versioner diffs consecutive specs.
We can't delete the old spec because that will destroy the API history and therefore versioning process.
This spec needs to be completely separate file from the previous spec as to retain the history.

Because there's more than one file now, we need an organization system.
A file hierarchy of sorts.
The specs should be stored as separate files in the same directory and chronologically ordered using alphanumerical file names.
The ordering of which specs to diff into the next version is decided by the names.

<br />


### Handler map

Now that we have two real specs[4].
We need a function to combine the different service interfaces into a data structure indexed by their version.
Below is real generated Haskell code doing exactly that.

```
example'handlerMap ::
  ( MonadIO m, MonadCatch m
  , V0.Example'Service meta0 m, V1.Example'Service meta1 m)
  => (xtra -> Hooks m () meta0) -> (xtra -> Hooks m () meta1)
  -> xtra
  -> Map
       Major
       (Minor, Request -> m (Either Response Response))
example'handlerMap hooks0 hooks1 xtra = R.fromList
    [ (0, (0, V0.example'handler hooks0 xtra))
    , (1, (0, V1.example'handler hooks1 xtra))
    ]
```

I won't cover the gritty details of type signature except for the last part, the output.

`Map Major (Minor, Request -> m (Either Response Response))`

Whenever a client sends a request, it includes a version.
The version decides which tuple to look up respective to the major verson.
Finally, it compares the minor version.
Equal or less than?
Good.
Greater than?
Bad and throw an error.

The second part of the tuple looks more web API-ish.

`Request -> m (Either Response Response)`

It takes a parsed Request and returns either an runtime error'ed Response or some completed Response.
Every Fluid handler boils down to this function.

Of course, this is all possible because of the first two arguments of `example'handlerMap`.
They contain the different service handlers for each version.
With the `Map` usage, different versioned services can share the same endpoint.

### Add-ons

The existence of add-ons reduces ramp-up time.
They also patch onto an existing API libraries easily.
This is because the core of Fluid does not specify exactly how to send or recieve data.
Add-ons fill the gap by generating boilerplate code between Fluid with HTTP server and client libraries.

In addition, Fluid doesn't hard code the `"pull"` configuration.
The configuration is intended to be plugged into a HTTP server or client as adaptor code.

There's a couple benefits from this.

1. Fluid's design is flexible enough where you aren't limited to which libraries to combine with.
2. Generated code is allowed to be used outside production environments.

So the spec's address information from `"pull"` is a partial lie.
The configuration generated isn't used anywhere by default.
You can really do whatever in regards to transmitting.

But you probably don't want to do just whatever.
You want to do what the `"pull"` says.
Add-ons bridge the gap in a dumb and opininated manner.
No ambiguity.
And if an add-on doesn't exist for a language or library, it's simple to implement one.

### Rest of the specs

For curiosity sake, here's the translations of the remaining specs.

```
{
  "fluid": { "major": 0, "minor": 0 },
  "types": [
    {
      "n": "Hello",
      "m": [{"greeting":"String"}, {"target":"String"}],
      "o": "String"
    },
    { "n": "Color", "e": ["Red","Green","Blue"] },
    { "n": "FavoriteColor", "o": "Color" }
  ],
  "pull": {
    "name": "Example",
    "meta": "Unit",
    "error": "Unit",
    "protocol": "http",
    "host": "localhost",
    "port": 8080,
    "path": "/"
  }
}
```
_<sup><sub>Figure: Spec 3, Version 1.1</sub></sup>_

<br />


```
{
  "fluid": { "major": 0, "minor": 0 },
  "types": [
    {
      "n": "Hello",
      "m": [{"greeting":"String"}, {"target":"String"}],
      "o": "String"
    },
    { "n": "Color", "e": ["Red","Green","Blue","Yellow"] },
    { "n": "FavoriteColor", "o": "Color" }
  ],
  "pull": {
    "name": "Example",
    "meta": "Unit",
    "error": "Unit",
    "protocol": "http",
    "host": "localhost",
    "port": 8080,
    "path": "/"
  }
}
```
_<sup><sub>Figure: Spec 4, Version 2.0</sub></sup>_

<br />

```
{
  "fluid": { "major": 0, "minor": 0 },
  "types": [
    {
      "n": "Hello",
      "m": [{"greeting":"String"}, {"target":"String"}],
      "o": "String"
    },
    { "n": "Color", "e": ["Red","Green","Blue","Yellow"] }
  ],
  "pull": {
    "name": "Example",
    "meta": "Unit",
    "error": "Unit",
    "protocol": "http",
    "host": "localhost",
    "port": 8080,
    "path": "/"
  }
}
```
_<sup><sub>Figure: Spec 5, Version 3.0</sub></sup>_

### Published specs

So earlier, I made a tiny, little-baby lie.
Specs don't have to be separate files.
It's just wise to do so for API owners.

For a published spec, which should be treated as read-only, the rules change.

A publicly exposed spec should only expose what's needed to generate client code or mock server.
There's no reason to make that a collection of files when it can be a JSON file holding an array of specs.
As mentioned way above, it's possible to force a version.
Published specs force versions.
Again, it's not a problem as a read-only file.
No touchy.

Here are the examples as a published spec in all its glory. Recall that version `1.0` is dropped.

```
[
	{
		"fluid": { "major": 0, "minor": 0 },
		"version": { "major": 0, "minor": 0 },
		"types": [
			{ "n": "Hello", "m": [{"target":"String"}], "o": "String" }
		],
		"pull": {
			"name": "Example",
			"meta": "Unit",
			"error": "Unit",
			"protocol": "http",
			"host": "localhost",
			"port": 8080,
			"path": "/"
		}
	},
	{
		"fluid": { "major": 0, "minor": 0 },
		"version": { "major": 1, "minor": 1 },
		"types": [
			{
				"n": "Hello",
				"m": [{"greeting":"String"}, {"target":"String"}],
				"o": "String"
			},
			{ "n": "Color", "e": ["Red","Green","Blue"] },
			{ "n": "FavoriteColor", "o": "Color" }
		],
		"pull": {
			"name": "Example",
			"meta": "Unit",
			"error": "Unit",
			"protocol": "http",
			"host": "localhost",
			"port": 8080,
			"path": "/"
		}
	},
	{
		"fluid": { "major": 0, "minor": 0 },
		"version": { "major": 2, "minor": 0 },
		"types": [
			{
				"n": "Hello",
				"m": [{"greeting":"String"}, {"target":"String"}],
				"o": "String"
			},
			{ "n": "Color", "e": ["Red","Green","Blue","Yellow"] },
			{ "n": "FavoriteColor", "o": "Color" }
		],
		"pull": {
			"name": "Example",
			"meta": "Unit",
			"error": "Unit",
			"protocol": "http",
			"host": "localhost",
			"port": 8080,
			"path": "/"
		}
	},
	{
		"fluid": { "major": 0, "minor": 0 },
		"version": { "major": 3, "minor": 0 },
		"types": [
			{
				"n": "Hello",
				"m": [{"greeting":"String"}, {"target":"String"}],
				"o": "String"
			},
			{ "n": "Color", "e": ["Red","Green","Blue","Yellow"] }
		],
		"pull": {
			"name": "Example",
			"meta": "Unit",
			"error": "Unit",
			"protocol": "http",
			"host": "localhost",
			"port": 8080,
			"path": "/"
		}
	}
]
```
_<sup><sub>Figure: Published Spec, Versions 0.0, 1.1, 2.0, 3.0</sub></sup>_

## What's next

### Language support

I want to target 40+ languages officially.
You can find the <a href="https://github.com/jxv/fluid/blob/master/targets.txt">full list in repo</a>.
Currently, only a full Haskell implementation exists because it was dogfooded with a <a href="https://www.camp47.com/s/blog">different project</a>.
The implementation serves as a nice template, but that still leaves a hefty amount of porting ahead.

On the flip side, the amount doesn't matter too much.
You can integrate with servers without an existing client implementation today.
It wasn't covered in this post, but the query in transport JSON is actually readable and fairly predictable.
This was intentional for the client on the outset.
So even if you downright hate IDLs, you'll never be stuck.

### Event sourcing

Another open-ended design decision is event sourcing.
In particular, I wanted to allow for CQRS.
As far as the spec is concerned, functions are types that happen to have output.
All such functions have no more than one input.
The inputs needing multiple arguments just use types with members.
The inputs needing a different kind or number of arguments use enumerals.
This is great because those the inputs (and outputs) can be uniformly logged and stored forever with their associated versions.
In reality, this area hasn't been touched.
So I only guarded against massive architectural changes.

### Documentation and Playground

Generating documentation is the clearest chasm to cross.
Having a spec format with a semantic parser is half-way there.
The dumbest generator will instantly add value.

On the other hand, the playground will need some creative love.
I'm imagining a generated web-client app for poking around with servers.
It could be something along the lines of Postman attached with documentation.
Or, it could be stricter with "views" for different versions and "combo boxed values" for enumerals. 
Nothing is set in stone.
The only requirement is that a `GET` with the playground to the Fluid endpoint should expose a field of dirty hacker possibilties.

### Contributions

If you wish contribute in a big or small way, <a href="http://httpapis.herokuapp.com">join the slack</a> channel #fluid.
There's plenty to do with the barrier of entry still low.
Feel free to contact me directly through my handle, `jxv`.
I'm also available through other slacks with the same handle.
I won't bite.

Thanks for reading!

<br/>
<br/>

[/r/haskell thread](https://www.reddit.com/r/haskell/comments/77rmxg/version_you_an_api_for_great_good/)

[/r/programming thread](https://www.reddit.com/r/programming/comments/77rvko/version_you_an_api_for_great_good/)

[HN thread](https://news.ycombinator.com/item?id=15521052)

<br/>
<br/>

<hr />

_Note: Fluid was previously named Colorless._

[1] That might sound scary. I recently talked at a local meetup and received plenty of related concerns from the audience. Their concerns have since been addressed with a reasonable solution. It's no longer scary. What that looks like pends as another post.

[2] People will hate on JSON even when abstracted. Sorry, it’s not my ideal either.

[3] GraphQL does something similar with its resolver.

[4] _spec win everytime_

[5] Function will be used often to mean service call. But not all functions are service calls.

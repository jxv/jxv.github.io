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
This is a benefit of IDLs.
It makes for a great pairing as you won’t be visibly stuck with JSON. 

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

The version can be placed somewhere as insider the header, possibly using a registered vendor MIME type. Or inside the request payload. Or even as a query parameter. Or as a subdomain. My favorite solution is simply to prefix an endpoint’s path with `/v1/` , `/v2/`, `/v3/`, etc. Nevertheless, the overall lack of consistency with placement is nothing short of ironic.

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
An IDL enforces that abstraction into language agnostic interface.
This interface is the spec.
With a generator, the spec creates a client side SDK and a server-side stubs.
It doesn’t stop there.
It the spec can create documentation as well.

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

We added another argument to the function `Hello`.
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

### Dispatch to service

### Type reuse

### Published specs

## Deprecating versions

### Dropping support

### Manual curation

## Fluid’s Implementation

Fluid applies all the theories described above.
In practice, specs are stored as separate files in the same directory and chronologically ordered using alphanumerical names.

### The spec

The Fluid spec format is in JSON, for pragmatic reasons.
Admittely, it was a hurdle to shoehorn, yet what turned out isn't terrible.
I think it's rather clean and legible format.
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


The key `"fluid"` describes the implementation version of the  transport.
It does not describe the API version.
It will be needed for when Fluid features are added, modified, or removed.

### Meta and Error

### Pull

### More specs

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

Notice there’s no `"version"` key in any of the specs.
It’s not required as the specs are automatically versioned as described in the previous section[tk].

### Handler map

### Add-ons

## What's next

### Language support

I want to target 40+ languages officially.
You can find the <a href="https://github.com/jxv/fluid/blob/master/targets.txt">full list in repo</a>.
Currently, only a full Haskell implementation exists because it was dogfooded with a <a href="https://www.camp47.com">different project</a>.
The implementation serves as a nice template, but that still leaves a lot porting ahead.

On the flip side, it doesn't matter too much.
You can today integrate with servers without an existing client implementation.
It wasn't covered in this post, but the query in transport JSON is actually readable and fairly predictable.
This was intentional for the client on the outset.
So even if you downright hate IDLs, you'll never be stuck.

### Event sourcing

Another open-ended design decision is event sourcing.
In particular, I wanted to allow for CQRS.
As far as the spec is concerned, functions are types that happen to have output.
And all functions have no more than one input.
The inputs needing multiple arguments just use types with members.
The inputs needing a different kind or number of arguments use enumerals.
Then the inputs (and outputs) can be logged and stored forever with their associated versions.
In reality, this area hasn't been touched.
So I only guarded for minimal architectural changes.

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

If you wish contribute in a big or small way, <a href="https://join.slack.com/t/fluid-idl/shared_invite/enQtMjU4NDA2NzM4MDM5LTc0NmM1NWE4M2Q5N2U4NDBjOTNmOTAzYTdmYzIyY2RhYTkxOTRjYjRiMjliOGNmOGQyN2ZmOWMwZTZiOTU0OTQ%22">join the slack</a>.
There's plenty to do with the barrier of entry still low.
Feel free to contact me directly through my handle, `jxv`.
I won't bite.

Thanks for reading!

<br/>
<br/>

<hr />

_Note: Fluid was previously named Colorless._

[1] That might sound scary. I recently talked at a local meetup and received plenty of related concerns from the audience. Their concerns have since been addressed with a reasonable solution. It's no longer scary. What that looks like pends as another post.

[2] People will hate on JSON even when abstracted. Sorry, it’s not my ideal either.

[3] GraphQL does something similar with its resolver.

[4] _spec win everytime_

[tk] You can cautiously add the version if you wish. Including key `"version"` with an object such as `{"major": 0, "minor": 0}` will force the API version of the spec. Be sure not to go backwards.

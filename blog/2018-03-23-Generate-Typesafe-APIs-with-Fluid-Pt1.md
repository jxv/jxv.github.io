# Generate Typesafe APIs with Fluid Pt.1

**March 23th, 2018**

[Fluid](https://github.com/jxv/fluid) is a web API generator.
It offers far more than type safety, and its goals aren't limited to Haskell.
First, let's dive into a Hello World and see how it works.

## Download the API generator

Go to the [website](https://www.fluid-idl.org) and download the executable.
If you're on Linux or OSX, rename the exectuable to `fluid` and save it in your path.
And if you're on Windows, I don't know. Do what you normally do. (Please tell me how you get it working!)

## Hello World Server

### Project Setup

The repo for this post is [here](https://github.com/jxv/hello-world-fluid-haskell-server) with commits for each version.
But you can follow along for future reference.

Create an empty project with stack and the `haskeleton` template.

```shell
stack new hello-world haskeleton
cd hello-world
```

Create directories for the specs.
```
mkdir api
mkdir api-json
```

### Initial Spec and Script

On the page of [Fluid's spec documentation](https://www.fluid-idl.org/spec),
there's two snippets needed to start with.

1. The spec for a Hello World API. Copy and paste that into a file.
2. A script for converting the specs from YAML to JSON. Fluid reads only JSON.


File: `api/000_init.yaml`
```yaml
fluid:
  major: 0
  minor: 0
pull:
  protocol: http
  name: HelloWorld
  host: localhost
  path: /
  port: 8080
  meta: Unit
  error: Unit
schema:
  Hello:
    m: [who: String]
    o: String
```


File: `api.sh`
```shell
#! /usr/bin/env sh

yaml2json () {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load($stdin.read))'
}

rm api-json/*
for yaml in api/*.yaml; do
    [ -f "$yaml" ] || break
    echo $yaml
    json=`basename $yaml .yaml`
    yaml2json < $yaml > "api-json/$json.json"
done
```

Then append the CLI command to generate.

```shell
fluid -l haskell -s api -m Api -n Api -d ./library -e server -a scotty
```

* `-l` is the language
* `-s` is the specs directory (or file)
* `-m` is the module name (eg. `This.Is.A.Module` is valid)
* `-n` is the name of the API directory to generate
* `-d` is the path of the API directory to generate
* `-e` is the side of code to generate -- `server` versus `client`.
* `-a` is optional and for all the add-ons (Just `scotty` in this case)

### Renaming and Dependencies

The `haskeleton` generated template project needs some trivial changes for a server.

Very quickly,

1. Move `library/Example.hs` to `library/HelloWorld.hs`.

2. In `executable/Main.hs` and `library/HelloWorld.hs`, replace instances of `Example` with `HelloWorld`.

3. Add these to `stack.yaml`'s `extra-deps`:
  
   * `fluid-idl-0.0.5`
   * `fluid-idl-scotty-0.0.0`

4. Add these `library`'s `dependencies` in `package.yaml`:
  
   * `fluid-idl`
   * `fluid-idl-scotty`
   * `mtl`
   * `scotty`
   * `text`

5. Lastly, add these to the `library`'s `default-extensions` in `package.yaml`:
   
   * `NamedFieldPuns`
   * `MultiParamTypeClasses`
   * `GeneralizedNewtypeDeriving`
   * `OverloadedStrings`

See if you're all set by compiling.

```shell
stack build
```

### Generate the API code

We just need to run the API script, whenever an addition or change to the specs is made.

```shell
./api.sh
```

With a single spec, just the two files, `library/Api/Server.hs` and `library/Api/Major0.hs`, are created.

* `library/Api/Server.hs`

This is the high level module of the API.
It exports the latest types, functions, services (monadic type class of functions), dispatch handlers, specs.
It also has the `scotty` add-ons for quick integration.

* `library/Api/Major0.hs`

This is first version (0.x) of the API.
This `Major` module exports the API's types, service (monadic type class of functions), a handler, and a spec.
With more specs to generate, there _could be_ more `Major` modules.
The author of the spec doesn't decide the version.
It's automatically decided and generated.
See [here](https://www.fluid-idl.org/evolution) for more detail.

You should be able to compile again.

```shell
stack build
```

### Integrate and Implement

Fluid generates the services to accommodate `mtl-style` architecture, as it's a widely adopted and maintainable pattern.

Below is a large rewrite of `library/HelloWorld.hs` with a functional server.
`App` is the newtype'd monad, where in a more complicated program would wrap environment and state data.
We just need an instance of `V0.HelloWorld'Service` over `App` to dispatch to the `Hello` function described in the spec.

File: `library/HelloWorld.hs`
```haskell
module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello

hello :: Monad m => V0.Hello -> m Text
hello V0.Hello{helloWho} = return $ "Hello " `mappend` helloWho `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

`V0.HelloWorld'Service` is a monadic type class, which contains all the functions defined in the schema.
The spec-to-code pattern follows from `FunctionName` to `serviceName'FunctionName` to avoid name collisions.
Something similar follows for types like `Hello`.
Its member `who` is prefixed with `hello`.
So it's `helloWho` in code.

`runServer` comes from `Fluid.Server.Scotty` as it runs the `ScottyT` transformer with the generated routes.

These lines are needed to pass error throws through Scotty into App.

```haskell
instance ServiceThrower App
instance V0.HelloWorld'Thrower App
```


Next, integrate the endpoints into a Scotty transformer.
The seconds argument of `helloWorld'Scotty'Post` contains callbacks for the API.
The type is called `Hooks`, and `defHooks` does the bare minimum satisfy that.
`const` ignores the HTTP headers passed from Scotty.

```haskell
routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

Compile and run!

```shell
stack build && stack exec hello-world
```

#### cURL'ing the API

A proper Fluid endpoint uses the same path for GET and POST.
GET is the discovery for the API. It returns the spec.
POST is where to target interaction with the API.

The absolute address for the API is `http://localhost:8080/`, as described in the (latest) spec.


Let's see what is the spec according to the server.

Request (in terminal):
```shell
curl http://localhost:8080/
```

Response:
```json
[
  {
    "fluid":{"minor":0,"major":0},
    "schema":{"Hello":{"m":[{"who":"String"}],"o":"String"}},
    "version":{"minor":0,"major":0},
    "pull":{"path":"/","error":"Unit","protocol":"http","name":"HelloWorld","meta":"Unit","host":"localhost","port":8080}
  }
]
```

Two things are different about the response compared to the original JSON spec, `api-json/000_init.json`.

1. The response is a JSON Array where each element is a represented spec.
   With more specs, the major versions are concatenated into one array.
2. A each element now has a `"version"` tag.
   The version was decided by Fluid.

Alright.

Now, let's call the service function, `Hello`.

Request:
```shell
curl \
  -XPOST \
  -H 'application/json' \
  -d '{
    "fluid":{"major":0,"minor":0},
    "version":{"major":0,"minor":0},
    "meta":null,
    "query":{"n":"Hello","m":{"who":"Joe"}}
  }' \
  http://localhost:8080
```

Response:
```json
{
  "tag":"Success",
  "success":"Hello Joe!",
  "limits":{"serviceCalls":50,"variables":50,"expressions":100,"lambdas":10}
}
```

It's a success.
```json
"Hello Joe!"
```

#### Hooks

Going back to the `Hooks` type,
they contain callbacks for transforming the `meta` type and limiting the sandboxed runtime.
`Hooks` are defined in `Fluid.Types` along with its default definition:

`Fluid.Types`
```haskell
data Hooks m meta meta' = Hooks
 { metaMiddleware :: meta -> m meta'
 , sandboxLimits :: meta' -> m Limits
 }

defHooks :: Monad m => Hooks m meta meta
defHooks = Hooks
 { metaMiddleware = return
 , sandboxLimits = \_ -> return defLimits
 }
```

With `defHooks`,
the `metaMiddleware` definition is an identity, and `sandboxLimits` is its default.

Notice the limits default are values in the previous POST response.

`Fluid.Types`
```
data Limits = Limits
  { variables :: Maybe Int
  , serviceCalls :: Maybe Int
  , lambdas :: Maybe Int
  , expressions :: Maybe Int
  } deriving (Show, Eq, Generic)

defLimits :: Limits
defLimits = Limits
  { variables = Just 50
  , serviceCalls = Just 50
  , lambdas = Just 10
  , expressions = Just 100
  }
```

### Adding a `Goodbye` function

The API users can say "Hello" now.
But! Nobody is allowed to leave because they can't say "Goodbye".
We should fix that and with another function in the API.
And it's easy to do.

Copy `api/000_init.yaml` and name it `api/001_goodbye.yaml`.

Append this function to the new schema.
```yaml
  Goodbye:
    m: [who: String]
    o: String
```

File: `api/001_goodbye.yaml`
```yaml
fluid:
  major: 0
  minor: 0
pull:
  protocol: http
  name: HelloWorld
  host: localhost
  path: /
  port: 8080
  meta: Unit
  error: Unit
schema:
  Hello:
    m: [who: String]
    o: String
  Goodbye:
    m: [who: String]
    o: String
```

Generate the new API code.

```shell
./api.sh
```

Peek into `library/Api/Major0.hs` and see that its version had a minor change to `0.1`.

The Service's instance needs a definition for `Goodbye` function.

```haskell
goodbye :: Monad m => V0.Goodbye -> m Text
goodbye V0.Goodbye{goodbyeWho} = return $ "Goodbye " `mappend` goodbyeWho `mappend` "!"
```

File: `library/HelloWorld.hs`
```haskell
module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello
  helloWorld'Goodbye () = goodbye

hello :: Monad m => V0.Hello -> m Text
hello V0.Hello{helloWho} = return $ "Hello " `mappend` helloWho `mappend` "!"

goodbye :: Monad m => V0.Goodbye -> m Text
goodbye V0.Goodbye{goodbyeWho} = return $ "Goodbye " `mappend` goodbyeWho `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

Request:
```shell
curl \
  -XPOST \
  -H 'application/json' \
  -d '{
    "fluid":{"major":0,"minor":0},
    "version":{"major":0,"minor":1},
    "meta":null,
    "query":{"n":"Goodbye","m":{"who":"Joe"}}
  }' \
  http://localhost:8080
```

Response:
```json
{
  "tag":"Success",
  "success":"Goodbye, Joe!",
  "limits":{"serviceCalls":50,"variables":50,"expressions":100,"lambdas":10}
}
```

## Add `Lang` to `Hello`

The world is a bigger place outside English speakers.
Let's add a couple more language greetings, Spanish and French, to `Hello`.
Modifying `Hello` will be a major version change.

```yaml
  Lang: [English, Spanish, French]
  Hello:
    m: [lang: Lang, who: String]
    o: String
```

File: `api/002_langs.yaml`
```yaml
fluid:
  major: 0
  minor: 0
pull:
  protocol: http
  name: HelloWorld
  host: localhost
  path: /
  port: 8080
  meta: Unit
  error: Unit
schema:
  Lang: [English, Spanish, French]
  Hello:
    m: [lang: Lang, who: String]
    o: String
  Goodbye:
    m: [who: String]
    o: String
```

Generate again.

```shell
./api.sh
```

This is the first major version change to `1.0`.
A few more things need to be added like a second definition for `Hello`.
That doesn't mean to delete the original `Hello` definition.

```haskell
hello' :: Monad m => V1.Hello -> m Text
hello' req = case V1.helloLang req of
  V1.Lang'English -> return $ "Hello " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'Spanish -> return $ "Hola " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'French -> return $ "Bonjour " `mappend` (V1.helloWho req) `mappend` "!"
```

Again, an instance for catching throws.

```haskell
instance V1.HelloWorld'Thrower App
```

The previous `Goodbye` definition can be reused as the type signature doesn't change.

```haskell
instance V1.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye
```

Add another `Hooks` for verison `1.x` with another `(const defHooks)`.

```haskell
routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks) (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

File: `library/HelloWorld.hs`
```haskell
module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0
import qualified Api.Major1 as V1

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App
instance V1.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello
  helloWorld'Goodbye () = goodbye

instance V1.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye

hello :: Monad m => V0.Hello -> m Text
hello req = return $ "Hello " `mappend` (V0.helloWho req) `mappend` "!"

hello' :: Monad m => V1.Hello -> m Text
hello' req = case V1.helloLang req of
  V1.Lang'English -> return $ "Hello " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'Spanish -> return $ "Hola " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'French -> return $ "Bonjour " `mappend` (V1.helloWho req) `mappend` "!"

goodbye :: Monad m => V0.Goodbye -> m Text
goodbye req = return $ "Goodbye " `mappend` (V0.goodbyeWho req) `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks) (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

Request:
```shell
curl \
  -XPOST \
  -H 'application/json' \
  -d '{
    "fluid":{"major":0,"minor":0},
    "version":{"major":1,"minor":0},
    "meta":null,
    "query":{"n":"Hello","m":{"lang":{"tag":"Spanish"},"who":"Joe"}}
  }' \
  http://localhost:8080
```

Response:
```json
{
  "tag":"Success",
  "success":"Hola Joe!",
  "limits":{"serviceCalls":50,"variables":50,"expressions":100,"lambdas":10}
}
```

## Add `Lang` to `Goodbye`

Let's release a new public API by adding Spanish and French greetings to `Goodbye` as well.
Modifying `Goodbye` will be another major version.

File: `api/003_goodbye_lang.yaml`
```yaml
fluid:
  major: 0
  minor: 0
pull:
  protocol: http
  name: HelloWorld
  host: localhost
  path: /
  port: 8080
  meta: Unit
  error: Unit
schema:
  Lang: [English, Spanish, French]
  Hello:
    m: [lang: Lang, who: String]
    o: String
  Goodbye:
    m: [lang: Lang, who: String]
    o: String
```

Generate.

```
./api.sh
```

The new `Goodbye` definition:

```haskell
goodbye' :: Monad m => V2.Goodbye -> m Text
goodbye' req = case V2.goodbyeLang req of
  V2.Lang'English -> return $ "Goodbye " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'Spanish -> return $ "Adios " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'French -> return $ "Adieu " `mappend` (V2.goodbyeWho req) `mappend` "!"
```


File: `library/HelloWorld.hs`
```haskell
module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0
import qualified Api.Major1 as V1
import qualified Api.Major2 as V2

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App
instance V1.HelloWorld'Thrower App
instance V2.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello
  helloWorld'Goodbye () = goodbye

instance V1.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye

instance V2.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye'

hello :: Monad m => V0.Hello -> m Text
hello req = return $ "Hello " `mappend` (V0.helloWho req) `mappend` "!"

hello' :: Monad m => V1.Hello -> m Text
hello' req = case V1.helloLang req of
  V1.Lang'English -> return $ "Hello " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'Spanish -> return $ "Hola " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'French -> return $ "Bonjour " `mappend` (V1.helloWho req) `mappend` "!"

goodbye :: Monad m => V0.Goodbye -> m Text
goodbye req = return $ "Goodbye " `mappend` (V0.goodbyeWho req) `mappend` "!"

goodbye' :: Monad m => V2.Goodbye -> m Text
goodbye' req = case V2.goodbyeLang req of
  V2.Lang'English -> return $ "Goodbye " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'Spanish -> return $ "Adios " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'French -> return $ "Adieu " `mappend` (V2.goodbyeWho req) `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks) (const defHooks) (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
```

Request:
```shell
curl \
  -XPOST \
  -H 'application/json' \
  -d '{
    "fluid":{"major":0,"minor":0},
    "version":{"major":2,"minor":0},
    "meta":null,
    "query":{"n":"Goodbye","m":{"lang":{"tag":"French"},"who":"Joe"}}
  }' \
  http://localhost:8080
```

Response:
```json
{
  "tag":"Success",
  "success":"Adieu Joe!",
  "limits":{"serviceCalls":50,"variables":50,"expressions":100,"lambdas":10}
}
```
## Meta middleware, again

It's worth covering the meta middleware again.
It didn't get a fair explanation above.
Primarly, it wasn't used.
The headers were ignored and the spec did not hold a meaingful `meta` type as `Unit`.

The middleware can be useful for injecting authentication information from either the header or meta value.
Authentication is common in  many requests, and this is the middleware to do it.
If nothing else, you can think of `meta` and the middleware use cases similar to HTTP headers and HTTP middlewares.

## Protocol: Pros and Cons

One of Fluid's beauties is that it doesn't dictate how data is transferred.
It's protocol agnostic.
It's on a layer above protocols.
It will run on any protocol as long as JSON can be sent.
And if you want to stick with HTTP but not Scotty, you can use Servant or Yesod too.
Fluid will run ontop of them.

Why move away from a protocol?
Aside from being cleaner as an abstraction,
there's an inglorious amount of trivial and manual work to create and maintain growing a HTTP RESTful API.
Fluid wants to do away with that but without burning the house down.
Fortunatley, REST principles can still be applied to the schema, HTTP can be still used as a transport, and JSON tools are still relevant.

Abstracting on HTTP, it comes at the cost.
Being protocol agnostic loses the benefit of cached HTTP responses.
The problem exists elsewhere with GraphQL.
GraphQL has a query language too, and caching can be resolved it by storing data on the client side too.
But in many and most cases, that's not important.
Discern if you're likely to serve large amounts of static data where proxy or protocol benefits are essential.
On the otherhand, if the data in responses are ever changing by nature and it's essential, caching will make no difference.

But given how easy and fast it is to use Fluid, trivialities for optimization doesn't make a lot of sense.
Additionally, neither approach limits one from the other.
That's the important factor to consider in the long term.
A mixed approach may be valid.
It's a tradeoff.

## What's next? (Part 2)

Fluid is techinically an IDL (Interface Description Language).
Generating server code is just half the equation.
Generating client code is the other half.
We'll explore how to do that next time.

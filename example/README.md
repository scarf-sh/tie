
Hi! This is a full top-to-bottom example for using Tie.

# Run the example with Cabal

Run the following command from the `example/` directory of the Tie repository:

```bash
$ cabal run
```

This will build and launch a webserver on port 8080.

# Re-generate the api code

Run the following command from the `example/` directory of the Tie repository:

```bash
$ tie --output generated --module-name Petstore.API --package-name petstore-api petstore.yaml
```

This will generate a Cabal package into the `generated` directory.

# Structure of the generated code

The generated code will placed in [`/generated`](/generated). The modules are placed under the `Petstore.API` (`PetStore/API`) Haskell module namespace.

  - [`/generated/Petstore/API/Api.hs`](/generated/Petstore/API/Api.hs) contains the API definition 
    for the Petstore. This file is derived from the operations defined in the OpenAPI specification.
    In particular, the operation names are derived from the `operationId` property of the Operations
    as defined in the specification.

  - [`/generated/Petstore/API/Schemas`](/generated/Petstore/API/Schemas) is where the schema 
    definitions are being placed by Tie. You can find the definition for `Pet` and `Pets` as well as 
    `Error` in here.

  - [`/generated/Petstore/API/Response`](/generated/Petstore/API/Response) is where Tie places the 
    response types for the individual operations. You will find a Haskell module for each operation 
    in the OpenAPI specification here.
{
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.url = "github:input-output-hk/empty-flake";
    };
  };
  outputs = _: { };
}

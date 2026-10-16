defmodule MetadataFixture.MixProject do
  use Mix.Project
  def project, do: [app: :metadata_fixture, version: "0.1.0", deps: [
    {:local_dep, path: "../local_dep"}
  ]]
end

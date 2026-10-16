defmodule MetadataFixture.MixProject do
  use Mix.Project
  def project do
    [app: :metadata_fixture, version: "VERSION", deps: [
      {:runtime_app, "~> 1.0", hex: :runtime_package},
      {:optional_dep, "~> 2.0", optional: true},
      {:dev_dep, "~> 3.0", only: :dev}
    ]]
  end
end

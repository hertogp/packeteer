defmodule Packeteer.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/hertogp/packeteer"

  def project do
    [
      app: :packeteer,
      version: @version,
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def docs do
    [
      main: "readme",
      extras: [
        "README.md": [title: "Readme"],
        "LICENSE.md": [title: "License"],
        "CHANGELOG.md": []
      ],
      source_url: @source_url,
      source_ref: "v#{@version}",
      formatters: ["html"]
      # assets: "assets"
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.34", only: :dev, runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:benchee, "~> 1.3", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end
end

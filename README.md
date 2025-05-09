## Proteus Compiler - V3 
 
This project marks a significant advancement over the V1 and V2 compilers by introducing a platform-independent compilation solution. 
We chose Scala for its ability to run on the Java Virtual Machine (JVM) and generate platform-agnostic Java bytecode.
We also addressed some bugs that appeared in V1 and V2. 

### Getting Started

To build this project on your machine, please follow these steps:

1. Install Scala: https://www.scala-lang.org/download/

2. Command-line setup (recommended for consistency): https://docs.scala-lang.org/getting-started/sbt-track/getting-started-with-scala-and-sbt-on-the-command-line.html

3. IDE Considerations (may vary): https://www.scala-sbt.org/1.x/docs/IDE.html
- IntelliJ: https://www.jetbrains.com/help/idea/sbt-support.html#sbt_scala_version_compatibility
- VS Code (Metals): https://scalameta.org/metals/docs/editors/vscode/

4. Clone the project on your local machine

5. Run `sbt compile`

### Contributing

We welcome contributions from anyone interested in improving this compiler!
To ensure the project's stability and correctness, we require that all contributions are verified to successfully pass `sbt compile` before submission.
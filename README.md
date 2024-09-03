# Better Error Compiler for OCaml

BEC (Better Error Compiler) is a tool designed to improve the error messages of the OCaml programming language.
This project builds on and enhances the artifact from the paper ["Getting Into the Flow"](https://dl.acm.org/doi/10.1145/3622812).
Our enhancements include backend support for unnamed (anonymous) functions. Additionally, we have focused on enhancing the user experience on the frontend (which resides in a separate repository) by integrating LLM support.
This allows the system to provide explanations of data flow results and offer solution suggestions to the user through API-triggered interactions with Gemini.

## Installation
To set up BEC server, You have two options:

1. **Manual**:
   * Clone the repository:
      ```Bash
      git clone https://github.com/RazanDally/BECompiler
      cd BEC
      ```
   * Build the project:

      Install Scala and sbt by following the Coursier installation instructions at [https://get-coursier.io/docs/cli-installation](https://get-coursier.io/docs/cli-installation).

       Run the following command to build the project:
      ```bash
      sbt compile
      ```
      ![sbt_compile_command](https://github.com/RazanDally/BECompiler/blob/main/installation_gallery/sbt_compile.png?raw=true)
      Building the project is only required the first time.
     
    * Run the following command to start the sbt terminal:
      ```
      sbt
      ```
      ![sbt_server_activation](https://github.com/RazanDally/BECompiler/blob/main/installation_gallery/sbt.png?raw=true)
    * Run the following command **in the sbt terminal** in order to start the server:
      ```
      run
      ```
      ![backend_server_activation](https://github.com/RazanDally/BECompiler/blob/main/installation_gallery/run.png?raw=true)
    * To deactivate the sbt terminal, type
       ```ps
        exit
        ```
        or use the command
        ```ps
        ctrl + c
        ```

2.  **Automatic**:
    * Follow the instructions provided in the [Frontend server](https://github.com/jouwana/BECompiler/blob/main/server/README.md) to automatically set up and run the server.

## Acknowledgments
This project was inspired by the article "Getting Into the Flow", which discusses improving the developer experience through better error handling.
Special thanks to the authors of the original artifact for providing a solid foundation on which we built these enhancements.

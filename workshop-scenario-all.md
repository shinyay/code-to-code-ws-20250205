# Legacy Code Modernization Workshop

This workshop is designed to help you understand the process of modernizing legacy code with using **GitHub Copilot**.

## Scenario

## Step 1: Ask GitHub Copilot how to migrate the code to a new language

### Task 1: Ask GitHub Copilot on GitHub Copilot Chat

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.
2. Ask GitHub Copilot for the following:

    ```plaintext
    When determining the scope of a COBOL to Java conversion project, how do I determine the scope of the code base, modules, and functionality my team needs to migrate? What are the steps involved?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
To determine the scope of the code base, modules, and functionality for a COBOL to a new language conversion project, follow these steps:

1. **Inventory the COBOL Codebase**:
   - Identify all COBOL programs, scripts, and related files.
   - Document the size of the codebase (e.g., lines of code, number of files).

2. **Identify Modules and Components**:
   - Break down the COBOL codebase into logical modules and components.
   - Document the purpose and functionality of each module.

3. **Analyze Dependencies**:
   - Identify dependencies between COBOL modules.
   - Document external dependencies (e.g., databases, third-party libraries).

4. **Assess Functionality**:
   - List all functionalities provided by the COBOL application.
   - Prioritize functionalities based on business importance.

5. **Evaluate Data Structures**:
   - Document data structures used in the COBOL application.
   - Identify data storage mechanisms (e.g., files, databases).

6. **Review Business Logic**:
   - Extract and document business rules and logic embedded in the COBOL code.

7. **Determine Integration Points**:
   - Identify integration points with other systems.
   - Document interfaces and communication protocols.

8. **Estimate Effort**:
   - Estimate the effort required to convert each module and functionality.
   - Consider complexity, size, and dependencies.

9. **Plan Testing and Validation**:
   - Define a testing strategy for the converted Java application.
   - Plan for unit tests, integration tests, and user acceptance tests.

10. **Create a Migration Roadmap**:
    - Develop a detailed migration plan with timelines and milestones.
    - Include risk assessment and mitigation strategies.

By following these steps, you can comprehensively determine the scope of the COBOL to Java conversion project and ensure a structured and efficient migration process.
</details>

### Task 2: Additional questions based on the GitHub Copilot Answer
1. Ask GitHub Copilot for the following:

    ```plaintext
    Thank you so much for your answer about COBOL migration. Now I would like to understand the each step in detail. Describe in detail the procedure for inventorying the COBOL code base. Also, please tell us what GitHub Copilot can assist in those steps.
    ```

<details>
<summary>2. GitHub Copilot Sample Response</summary>
Sure, here is a detailed procedure for inventorying the COBOL code base, along with how GitHub Copilot can assist in those steps:

### Step-by-Step Procedure for Inventorying the COBOL Code Base

1. **Identify All COBOL Programs and Scripts**:
   - Locate all directories and subdirectories containing COBOL source files.
   - Identify file extensions commonly used for COBOL programs (e.g., `.cbl`, `.cob`, `.cpy`).

2. **Catalog the Files**:
   - Create a list of all COBOL files, including their paths.
   - Use scripts or tools to automate the file listing process.

3. **Document File Metadata**:
   - Record metadata for each file, such as file size, last modified date, and author (if available).
   - Use version control system logs to gather historical data.

4. **Analyze Code Size**:
   - Calculate the total lines of code (LOC) for the entire codebase.
   - Break down LOC by individual files and modules.

5. **Identify and Document Dependencies**:
   - Identify dependencies between COBOL programs (e.g., copybooks, called programs).
   - Document external dependencies, such as database connections or third-party libraries.

6. **Classify Files by Functionality**:
   - Group files based on their functionality (e.g., data processing, user interface, reporting).
   - Create a high-level overview of the application structure.

7. **Review and Validate Inventory**:
   - Review the inventory with team members to ensure completeness and accuracy.
   - Validate the inventory against the actual codebase to identify any missing files.

### How GitHub Copilot Can Assist

1. **Automate File Listing**:
   - GitHub Copilot can help write scripts to list all COBOL files in the directory structure.
   - Example script in Python:

   ```python
   // filepath: /path/to/inventory_script.py
   import os

   def list_cobol_files(directory):
       cobol_files = []
       for root, dirs, files in os.walk(directory):
           for file in files:
               if file.endswith(('.cbl', '.cob', '.cpy')):
                   cobol_files.append(os.path.join(root, file))
       return cobol_files

   directory = '/path/to/cobol/codebase'
   cobol_files = list_cobol_files(directory)
   for file in cobol_files:
       print(file)
   ```

2. **Calculate Lines of Code**:
   - GitHub Copilot can assist in writing scripts to calculate LOC for each file.
   - Example script in Python:

   ```python
   // filepath: /path/to/loc_script.py
   def count_lines_of_code(file_path):
       with open(file_path, 'r') as file:
           return len(file.readlines())

   total_loc = 0
   for file in cobol_files:
       total_loc += count_lines_of_code(file)

   print(f'Total Lines of Code: {total_loc}')
   ```

3. **Extract Metadata**:
   - GitHub Copilot can help write scripts to extract file metadata.
   - Example script in Python:

   ```python
   // filepath: /path/to/metadata_script.py
   import os
   import time

   def get_file_metadata(file_path):
       file_stats = os.stat(file_path)
       return {
           'size': file_stats.st_size,
           'last_modified': time.ctime(file_stats.st_mtime),
           'author': 'unknown'  # Placeholder, as author info may not be available
       }

   for file in cobol_files:
       metadata = get_file_metadata(file)
       print(f'{file}: {metadata}')
   ```

4. **Identify Dependencies**:
   - GitHub Copilot can assist in writing scripts to identify dependencies within COBOL files.
   - Example script in Python:

   ```python
   // filepath: /path/to/dependency_script.py
   def find_dependencies(file_path):
       dependencies = []
       with open(file_path, 'r') as file:
           for line in file:
               if 'COPY' in line or 'CALL' in line:
                   dependencies.append(line.strip())
       return dependencies

   for file in cobol_files:
       dependencies = find_dependencies(file)
       print(f'{file} dependencies: {dependencies}')
   ```

By leveraging GitHub Copilot to automate these tasks, you can efficiently inventory the COBOL code base and gather the necessary information for the migration project.
</details>

### Task 3: Ask GitHub Copilot about Benefits of Code to Doc / Doc to Code

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

    ```plaintext
    Speaking of COBOL migration, I am aware that there is such a thing as a tool to convert COBOL to multiple languages. What are the advantages of using GitHub Copilot to migrate code based on an understanding of the COBOL code base compared to using such a conversion tool?
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Using GitHub Copilot to migrate COBOL code based on an understanding of the COBOL code base has several advantages compared to using automated conversion tools. Here are some key advantages:

### 1. **Customization and Flexibility**
   - **Tailored Solutions**: GitHub Copilot allows you to write custom code that is tailored to your specific requirements and business logic, rather than relying on generic conversion rules.
   - **Flexibility**: You can make design decisions and optimizations that are specific to your application's needs, which may not be possible with automated tools.

### 2. **Code Quality and Maintainability**
   - **Readable Code**: Code generated with GitHub Copilot is often more readable and maintainable, as it follows standard coding practices and conventions of the target language.
   - **Refactoring**: You can refactor and improve the code during the migration process, ensuring that the resulting codebase is clean and maintainable.

### 3. **Understanding and Control**
   - **Deep Understanding**: By manually migrating the code with the help of GitHub Copilot, you gain a deeper understanding of both the original COBOL code and the new codebase, which is valuable for future maintenance and enhancements.
   - **Control Over Migration**: You have full control over the migration process, allowing you to handle edge cases, complex business logic, and specific requirements that automated tools might miss.

### 4. **Error Handling and Debugging**
   - **Custom Error Handling**: You can implement custom error handling and logging mechanisms that are appropriate for your application, rather than relying on generic error handling provided by automated tools.
   - **Debugging**: Manually migrating the code allows you to thoroughly test and debug each part of the application, ensuring that it works correctly in the new environment.

### 5. **Integration with Modern Technologies**
   - **Modern Practices**: You can integrate modern development practices, frameworks, and libraries during the migration process, which may not be possible with automated tools.
   - **Seamless Integration**: You can ensure seamless integration with other parts of your system, including databases, APIs, and third-party services.

### 6. **Incremental Migration**
   - **Step-by-Step Migration**: GitHub Copilot allows you to migrate the code incrementally, testing and validating each part before moving on to the next. This reduces the risk of introducing errors and makes the migration process more manageable.
   - **Parallel Development**: You can continue to develop and maintain the existing COBOL application while gradually migrating to the new language.

### Example: Migrating a COBOL Subroutine to Python with GitHub Copilot

#### COBOL Subroutine
```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```

#### Python Function with GitHub Copilot
```python
class Employee:
    def __init__(self, emp_id, name, department, gross_salary):
        self.emp_id = emp_id
        self.name = name
        self.department = department
        self.gross_salary = gross_salary

def initialize_employees():
    employees = []
    employees.append(Employee("E001", "Alice Johnson", "HR", 70000.00))
    employees.append(Employee("E002", "Bob Smith", "IT", 85000.00))
    employees.append(Employee("E003", "Charlie Brown", "Finance", 60000.00))
    employees.append(Employee("E004", "David Wilson", "Marketing", 75000.00))
    employees.append(Employee("E005", "Eve Davis", "HR", 75000.00))
    return employees

# Example usage
employees = initialize_employees()
for emp in employees:
    print(f"ID: {emp.emp_id}, Name: {emp.name}, Department: {emp.department}, Gross Salary: {emp.gross_salary}")
```

### Conclusion
While automated conversion tools can be useful for quickly converting large codebases, using GitHub Copilot to manually migrate COBOL code provides greater customization, control, and understanding of the resulting code. This approach ensures higher code quality, maintainability, and the ability to integrate modern development practices and technologies.
</details>

### Task 4: Ask GitHub Copilot about the automatic conversion tool

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

    ```plaintext
    What are the possible advantages and disadvantages of using an automatic conversion tool?
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Using an automatic conversion tool to migrate COBOL code to another language has its own set of advantages and disadvantages. Here are some of the key points to consider:

### Advantages

1. **Speed and Efficiency**
   - **Quick Conversion**: Automatic tools can convert large codebases quickly, saving significant time compared to manual migration.
   - **Batch Processing**: These tools can handle batch processing of multiple files, making the migration process more efficient.

2. **Consistency**
   - **Uniform Code**: The converted code is consistent in style and structure, as it follows predefined rules and patterns.
   - **Reduced Human Error**: Automated tools reduce the risk of human error during the conversion process.

3. **Cost-Effective**
   - **Lower Initial Cost**: The initial cost of using an automated tool can be lower than hiring a team of developers to manually migrate the code.
   - **Resource Savings**: Saves resources by reducing the need for extensive manual labor.

4. **Legacy System Support**
   - **Specialized Tools**: Some tools are specifically designed to handle legacy COBOL code and can manage complex COBOL constructs and data types.

### Disadvantages

1. **Code Quality and Maintainability**
   - **Readability Issues**: The converted code may be difficult to read and maintain, as it might not follow best practices of the target language.
   - **Technical Debt**: The resulting code may contain technical debt, requiring significant refactoring and optimization.

2. **Limited Customization**
   - **Generic Conversion**: Automated tools may not handle specific business logic or custom requirements effectively, leading to potential issues in functionality.
   - **Lack of Flexibility**: Limited ability to customize the conversion process to suit specific needs.

3. **Error Handling and Debugging**
   - **Hidden Bugs**: The converted code may contain hidden bugs that are difficult to identify and fix.
   - **Complex Debugging**: Debugging issues in automatically converted code can be challenging due to the lack of understanding of the generated code.

4. **Integration Challenges**
   - **Modern Practices**: Automated tools may not integrate modern development practices, frameworks, or libraries, leading to outdated code.
   - **System Integration**: Ensuring seamless integration with other parts of the system may require additional manual effort.

5. **Dependency on Tool**
   - **Vendor Lock-In**: Relying on a specific tool may lead to vendor lock-in, making future migrations or changes dependent on the tool's availability and support.
   - **Tool Limitations**: The tool may have limitations in handling certain COBOL constructs or specific target language features.

### Example: Automatic Conversion Tool vs. Manual Migration with GitHub Copilot

#### Automatic Conversion Tool
- **Pros**: Quick conversion, consistent code, reduced initial cost.
- **Cons**: Potential readability issues, limited customization, hidden bugs, integration challenges.

#### Manual Migration with GitHub Copilot
- **Pros**: Customizable, higher code quality, better understanding and control, modern practices integration.
- **Cons**: Time-consuming, requires skilled developers, higher initial cost.

### Conclusion
While automatic conversion tools offer speed and efficiency, they come with trade-offs in terms of code quality, customization, and maintainability. Manual migration with tools like GitHub Copilot provides greater control and flexibility, ensuring a more robust and maintainable codebase. The choice between the two approaches depends on the specific requirements, resources, and goals of the migration project.
</details>

### Task 5: Key COBOL Terminology that You should know

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

    ```plaintext
    I recognize that COBOL has a number of technical terms that are unique to COBOL, such as subroutines, copy clauses, and so on. As you consider converting COBOL to another language, please list the terminology you need to know and explain each one.
    ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>
Certainly! Here is a list of key COBOL terminology that you should know when considering converting COBOL to another language, along with explanations for each term:

### 1. **IDENTIFICATION DIVISION**
   - This section contains the program's identification information, such as the program name, author, and date.

### 2. **ENVIRONMENT DIVISION**
   - This section describes the environment in which the program runs, including file assignments and external device specifications.

### 3. **DATA DIVISION**
   - This section defines the data used by the program. It is divided into several sections:
     - **WORKING-STORAGE SECTION**: Defines variables used within the program.
     - **FILE SECTION**: Defines the layout of records in files.
     - **LINKAGE SECTION**: Defines data passed to the program from external programs.

### 4. **PROCEDURE DIVISION**
   - This section contains the program's logic and procedures. It includes the actual code that performs the program's operations.

### 5. **COPYBOOK**
   - A reusable piece of code defined in a separate file. It is included in the program using the `COPY` statement.

### 6. **PERFORM**
   - A statement used to execute subroutines or loops. It is used to repeat specific operations.

### 7. **CALL**
   - A statement used to call external programs or subroutines, passing arguments to them.

### 8. **SECTION**
   - A logical grouping of related statements within the PROCEDURE DIVISION.

### 9. **PARAGRAPH**
   - A smaller unit within a SECTION, grouping a series of statements that perform a specific task.

### 10. **PIC (PICTURE)**
   - A clause that defines the format of a data item, specifying its type and size.
     - Example: `PIC 9(5)` represents a numeric field with 5 digits.

### 11. **OCCURS**
   - A clause used to define arrays, indicating that a data item repeats a specified number of times.

### 12. **VALUE**
   - A clause used to set the initial value of a data item.

### 13. **COMPUTE**
   - A statement used to perform arithmetic operations and store the result in a variable.

### 14. **IF**
   - A statement used for conditional branching, executing different code based on specified conditions.

### 15. **MOVE**
   - A statement used to copy data from one variable to another.

### 16. **STOP RUN**
   - A statement used to terminate the program's execution.

### 17. **DISPLAY**
   - A statement used to output messages or data to the screen.

### 18. **ACCEPT**
   - A statement used to receive input from the user.

### 19. **SORT**
   - A statement used to sort data in a specified order.

### 20. **SEARCH**
   - A statement used to search for specific elements within an array.

### 21. **SUBROUTINE**
   - A reusable block of code that can be called from different parts of the program. In COBOL, subroutines are often implemented using the `PERFORM` or `CALL` statements.

### 22. **COPY Clause**
   - A clause used to include the contents of a copybook into the program. This promotes code reuse and modularity.

### 23. **LEVEL NUMBERS**
   - Numbers used to define the hierarchical structure of data items. Common level numbers include 01, 05, 10, etc.

### 24. **REDEFINES**
   - A clause used to allow different data descriptions for the same storage area, enabling multiple views of the same data.

### 25. **88 LEVEL**
   - A special level number used to define condition names, which are used for readability and to simplify conditional logic.

Understanding these terms will help you effectively convert COBOL programs to another language by providing insight into the structure and functionality of COBOL code. Each term represents a fundamental concept or construct in COBOL programming, and knowing how they translate to constructs in the target language is crucial for a successful conversion.
</details>

## Step 2: Analyze the COBOL Code

### Task 1: Analyze the Code Dependency

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    #file:main.cobc Analyze the dependencies required to execute this code.
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
To analyze the dependencies required to execute the COBOL code in `main.cobc-1`, follow these steps:

### 1. **Internal Dependencies**:
   - **Data Structures**: The code relies on various data structures defined in the `WORKING-STORAGE SECTION`.
   - **Subroutines**: The `PROCEDURE DIVISION` contains several subroutines that are called within the `MAIN-PROCEDURE`.

### 2. **External Dependencies**:
   - **Called Programs**: The code calls an external COBOL program `CalculateBonus` using the `CALL` statement.

### Detailed Analysis:

#### Internal Dependencies:
1. **Data Structures**:
   - `MAX-EMPLOYEES`: Defines the maximum number of employees.
   - `EMPLOYEE-TABLE`: Stores employee details.
   - `SORTED-EMPLOYEE-TABLE`: Stores sorted employee details.
   - `DEPARTMENT-TOTALS`: Stores department-wise total salaries.
   - Various other variables like `EMPLOYEE-INDEX`, `INNER-INDEX`, `TAX-RATE`, etc.

2. **Subroutines**:
   - `INITIALIZE-EMPLOYEES`: Initializes employee data.
   - `SORT-EMPLOYEES`: Sorts employees based on their IDs.
   - `CALCULATE-NET-SALARIES`: Calculates net salaries for employees.
   - `CALCULATE-DEPARTMENT-TOTALS`: Calculates total salaries for each department.
   - `DISPLAY-EMPLOYEES`: Displays employee payroll information.
   - `DISPLAY-DEPARTMENT-TOTALS`: Displays department salary totals.

#### External Dependencies:
1. **Called Programs**:
   - `CalculateBonus`: This program is called to calculate the bonus for each employee. The `CALL` statement in `CALCULATE-NET-SALARIES` subroutine indicates this dependency.

### Example of External Dependency:
```cobol
...
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
               COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
               COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
           END-PERFORM.
...
```

### Summary:
To execute the code in `main.cobc-1`, ensure the following dependencies are met:
- All data structures and variables are correctly defined and initialized.
- All internal subroutines (`INITIALIZE-EMPLOYEES`, `SORT-EMPLOYEES`, etc.) are implemented.
- The external program `CalculateBonus` is available and correctly implemented.

By ensuring these dependencies are met, the `main.cobc-1` program can execute successfully.
</details>

### Task 2: Explain the code

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    @workspace /explain #file:main.cobc in detail.
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Sure, let's go through the provided COBOL code in detail.

### Identification Division
This section identifies the program.

```cobol
IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.
```

### Data Division
This section defines the data structures and variables used in the program.

#### Working-Storage Section
This section contains variables that are used throughout the program.

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.
```

- `MAX-EMPLOYEES` is a numeric variable with a maximum value of 999, initialized to 5.

#### Employee Table
This table stores employee details.

```cobol
       01  EMPLOYEE-TABLE.
           05  EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  EMPLOYEE-ID        PIC X(5).
               10  EMPLOYEE-NAME      PIC X(20).
               10  DEPARTMENT         PIC X(10).
               10  GROSS-SALARY       PIC 9(7)V99.
               10  BONUS              PIC 9(5)V99.
               10  DEDUCTIONS         PIC 9(5)V99.
               10  NET-SALARY         PIC 9(7)V99.
               10  TAX-DEDUCTION      PIC 9(5)V99.
```

- `EMPLOYEE-ENTRY` occurs 5 times, each entry containing details like `EMPLOYEE-ID`, `EMPLOYEE-NAME`, `DEPARTMENT`, `GROSS-SALARY`, `BONUS`, `DEDUCTIONS`, `NET-SALARY`, and `TAX-DEDUCTION`.

#### Sorted Employee Table
This table stores sorted employee details.

```cobol
       01  SORTED-EMPLOYEE-TABLE.
           05  SORTED-EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  SORT-EMPLOYEE-ID   PIC X(5).
               10  SORT-EMPLOYEE-NAME PIC X(20).
               10  SORT-DEPARTMENT    PIC X(10).
               10  SORT-GROSS-SALARY  PIC 9(7)V99.
               10  SORT-BONUS         PIC 9(5)V99.
               10  SORT-DEDUCTIONS    PIC 9(5)V99.
               10  SORT-NET-SALARY    PIC 9(7)V99.
               10  SORT-TAX-DEDUCTION PIC 9(5)V99.
```

- Similar to `EMPLOYEE-TABLE`, but used for sorted data.

#### Department Totals
This table stores total salaries for each department.

```cobol
       01  DEPARTMENT-TOTALS.
           05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
               10  DEPT-NAME          PIC X(10).
               10  TOTAL-SALARY       PIC 9(7)V99.
```

- `DEPARTMENT-TOTAL` occurs 5 times, each entry containing `DEPT-NAME` and `TOTAL-SALARY`.

#### Additional Variables
These variables are used for various calculations and temporary storage.

```cobol
       01  EMPLOYEE-INDEX           PIC 9(3).
       01  INNER-INDEX              PIC 9(3).
       01  TAX-RATE                 PIC 9V99 VALUE 0.20.
       01  BONUS-RATE               PIC 9V99 VALUE 0.10.
       01  DEDUCTION-RATE           PIC 9V99 VALUE 0.05.
       01  DEPARTMENT-INDEX         PIC 9(3).

       01  TEMP-ID                  PIC X(5).
       01  TEMP-NAME                PIC X(20).
       01  TEMP-DEPARTMENT          PIC X(10).
       01  TEMP-SALARY              PIC 9(7)V99.

       01  LNK-GROSS-SALARY         PIC 9(7)V99.
       01  LNK-BONUS                PIC 9(5)V99.
```

### Procedure Division
This section contains the executable code.

#### Main Procedure
The main entry point of the program.

```cobol
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-EMPLOYEES.
           PERFORM SORT-EMPLOYEES.
           PERFORM CALCULATE-NET-SALARIES.
           PERFORM CALCULATE-DEPARTMENT-TOTALS.
           PERFORM DISPLAY-EMPLOYEES.
           PERFORM DISPLAY-DEPARTMENT-TOTALS.
           STOP RUN.
```

- Calls various sub-procedures to initialize, sort, calculate, and display data.

#### Initialize Employees
Initializes the employee data.

```cobol
       INITIALIZE-EMPLOYEES.
           MOVE "E001" TO EMPLOYEE-ID(1).
           MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
           MOVE "HR" TO DEPARTMENT(1).
           MOVE 70000.00 TO GROSS-SALARY(1).
           ...
           MOVE "E005" TO EMPLOYEE-ID(5).
           MOVE "Eve Davis" TO EMPLOYEE-NAME(5).
           MOVE "HR" TO DEPARTMENT(5).
           MOVE 75000.00 TO GROSS-SALARY(5).
```

- Moves hardcoded values to the `EMPLOYEE-TABLE`.

#### Sort Employees
Sorts the employees by their IDs.

```cobol
       SORT-EMPLOYEES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
           ...
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX >= MAX-EMPLOYEES
               ...
               END-PERFORM.
```

- Copies data from `EMPLOYEE-TABLE` to `SORTED-EMPLOYEE-TABLE`.
- Sorts the `SORTED-EMPLOYEE-TABLE` using a bubble sort algorithm.

#### Calculate Net Salaries
Calculates the net salary for each employee.

```cobol
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- Uses a subroutine `CalculateBonus` to compute bonuses.
- Computes deductions, tax, and net salary.

#### Calculate Department Totals
Calculates total salaries for each department.

```cobol
       CALCULATE-DEPARTMENT-TOTALS.
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
               MOVE 0 TO TOTAL-SALARY(DEPARTMENT-INDEX)
           END-PERFORM.
           ...
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               ...
               END-PERFORM.
```

- Initializes department totals.
- Aggregates net salaries by department.

#### Display Employees
Displays employee details.

```cobol
       DISPLAY-EMPLOYEES.
           DISPLAY "Employee Payroll Information".
           DISPLAY "-----------------------------".
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- Displays sorted employee details.

#### Display Department Totals
Displays total salaries for each department.

```cobol
       DISPLAY-DEPARTMENT-TOTALS.
           DISPLAY "Department Salary Totals".
           DISPLAY "-----------------------------".
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
                   DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
                   ...
               END-IF
           END-PERFORM.
```

- Displays department totals if the department name is not empty.

This COBOL program is a payroll system that initializes employee data, sorts it, calculates net salaries, aggregates department totals, and displays the results.
</details>

### Task 3: Identify the Subroutines

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    #file:main.cobc What are the subroutines defined in this code?
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
The subroutines defined in the main.cobc code are:

1. **MAIN-PROCEDURE**
   - This is the main entry point of the program and calls other subroutines in sequence.

2. **INITIALIZE-EMPLOYEES**
   - Initializes the employee data with hardcoded values.

3. **SORT-EMPLOYEES**
   - Sorts the employees based on their IDs using a bubble sort algorithm.

4. **CALCULATE-NET-SALARIES**
   - Calculates the net salaries for employees by calling the `CalculateBonus` program and computing deductions and tax.

5. **CALCULATE-DEPARTMENT-TOTALS**
   - Calculates total salaries for each department by aggregating net salaries.

6. **DISPLAY-EMPLOYEES**
   - Displays the sorted employee details.

7. **DISPLAY-DEPARTMENT-TOTALS**
   - Displays the total salaries for each department.

These subroutines are called in sequence from the `MAIN-PROCEDURE` to perform the various tasks required by the payroll system.
</details>

4. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    Explain each subrouines for #file:main.cobc in detail and explain the relationship of each subroutines among subroutines.
    ```
<details>
<summary>5. GitHub Copilot Sample Response</summary>
Sure, let's explain each subroutine in detail and their relationships in the main.cobc file.

### Subroutines in main.cobc

1. **MAIN-PROCEDURE**
   - **Purpose**: This is the main entry point of the program. It orchestrates the execution of other subroutines.
   - **Details**: It sequentially calls the subroutines to initialize data, sort employees, calculate net salaries, calculate department totals, and display the results.
   - **Code**:
     ```cobol
     MAIN-PROCEDURE.
         PERFORM INITIALIZE-EMPLOYEES.
         PERFORM SORT-EMPLOYEES.
         PERFORM CALCULATE-NET-SALARIES.
         PERFORM CALCULATE-DEPARTMENT-TOTALS.
         PERFORM DISPLAY-EMPLOYEES.
         PERFORM DISPLAY-DEPARTMENT-TOTALS.
         STOP RUN.
     ```

2. **INITIALIZE-EMPLOYEES**
   - **Purpose**: Initializes the employee data with hardcoded values.
   - **Details**: Moves predefined values into the `EMPLOYEE-TABLE`.
   - **Code**:
     ```cobol
     INITIALIZE-EMPLOYEES.
         MOVE "E001" TO EMPLOYEE-ID(1).
         MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
         MOVE "HR" TO DEPARTMENT(1).
         MOVE 70000.00 TO GROSS-SALARY(1).
         ...
         MOVE "E005" TO EMPLOYEE-ID(5).
         MOVE "Eve Davis" TO EMPLOYEE-NAME(5).
         MOVE "HR" TO DEPARTMENT(5).
         MOVE 75000.00 TO GROSS-SALARY(5).
     ```

3. **SORT-EMPLOYEES**
   - **Purpose**: Sorts the employees based on their IDs.
   - **Details**: Copies data from `EMPLOYEE-TABLE` to `SORTED-EMPLOYEE-TABLE` and sorts it using a bubble sort algorithm.
   - **Code**:
     ```cobol
     SORT-EMPLOYEES.
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
             MOVE EMPLOYEE-NAME(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
             MOVE DEPARTMENT(EMPLOYEE-INDEX) TO SORT-DEPARTMENT(EMPLOYEE-INDEX)
             MOVE GROSS-SALARY(EMPLOYEE-INDEX) TO SORT-GROSS-SALARY(EMPLOYEE-INDEX)
         END-PERFORM.

         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX >= MAX-EMPLOYEES
             PERFORM VARYING INNER-INDEX FROM 1 BY 1 UNTIL INNER-INDEX > MAX-EMPLOYEES - EMPLOYEE-INDEX
                 IF SORT-EMPLOYEE-ID(INNER-INDEX) > SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                     MOVE SORT-EMPLOYEE-ID(INNER-INDEX) TO TEMP-ID
                     MOVE SORT-EMPLOYEE-NAME(INNER-INDEX) TO TEMP-NAME
                     MOVE SORT-DEPARTMENT(INNER-INDEX) TO TEMP-DEPARTMENT
                     MOVE SORT-GROSS-SALARY(INNER-INDEX) TO TEMP-SALARY
                     MOVE SORT-EMPLOYEE-ID(INNER-INDEX + 1) TO SORT-EMPLOYEE-ID(INNER-INDEX)
                     MOVE SORT-EMPLOYEE-NAME(INNER-INDEX + 1) TO SORT-EMPLOYEE-NAME(INNER-INDEX)
                     MOVE SORT-DEPARTMENT(INNER-INDEX + 1) TO SORT-DEPARTMENT(INNER-INDEX)
                     MOVE SORT-GROSS-SALARY(INNER-INDEX + 1) TO SORT-GROSS-SALARY(INNER-INDEX)
                     MOVE TEMP-ID TO SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                     MOVE TEMP-NAME TO SORT-EMPLOYEE-NAME(INNER-INDEX + 1)
                     MOVE TEMP-DEPARTMENT TO SORT-DEPARTMENT(INNER-INDEX + 1)
                     MOVE TEMP-SALARY TO SORT-GROSS-SALARY(INNER-INDEX + 1)
                 END-IF
             END-PERFORM
         END-PERFORM.
     ```

4. **CALCULATE-NET-SALARIES**
   - **Purpose**: Calculates the net salaries for employees.
   - **Details**: Calls the `CalculateBonus` program to compute bonuses and then calculates deductions, tax, and net salary.
   - **Code**:
     ```cobol
     CALCULATE-NET-SALARIES.
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
             CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
             MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
             COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
             COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
             COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
         END-PERFORM.
     ```

5. **CALCULATE-DEPARTMENT-TOTALS**
   - **Purpose**: Calculates total salaries for each department.
   - **Details**: Initializes department totals and aggregates net salaries by department.
   - **Code**:
     ```cobol
     CALCULATE-DEPARTMENT-TOTALS.
         PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
             MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
             MOVE 0 TO TOTAL-SALARY(DEPARTMENT-INDEX)
         END-PERFORM.

         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
                 IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES OR DEPT-NAME(DEPARTMENT-INDEX) = SORT-DEPARTMENT(EMPLOYEE-INDEX)
                     IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES
                         MOVE SORT-DEPARTMENT(EMPLOYEE-INDEX) TO DEPT-NAME(DEPARTMENT-INDEX)
                     END-IF
                     ADD SORT-NET-SALARY(EMPLOYEE-INDEX) TO TOTAL-SALARY(DEPARTMENT-INDEX)
                     EXIT PERFORM
                 END-IF
             END-PERFORM
         END-PERFORM.
     ```

6. **DISPLAY-EMPLOYEES**
   - **Purpose**: Displays the sorted employee details.
   - **Details**: Iterates through the sorted employee table and displays each employee's details.
   - **Code**:
     ```cobol
     DISPLAY-EMPLOYEES.
         DISPLAY "Employee Payroll Information".
         DISPLAY "-----------------------------".
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
             DISPLAY "Name: " SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
             DISPLAY "Department: " SORT-DEPARTMENT(EMPLOYEE-INDEX)
             DISPLAY "Gross Salary: $" SORT-GROSS-SALARY(EMPLOYEE-INDEX)
             DISPLAY "Bonus: $" SORT-BONUS(EMPLOYEE-INDEX)
             DISPLAY "Deductions: $" SORT-DEDUCTIONS(EMPLOYEE-INDEX)
             DISPLAY "Tax Deduction: $" SORT-TAX-DEDUCTION(EMPLOYEE-INDEX)
             DISPLAY "Net Salary: $" SORT-NET-SALARY(EMPLOYEE-INDEX)
             DISPLAY "-----------------------------"
         END-PERFORM.
     ```

7. **DISPLAY-DEPARTMENT-TOTALS**
   - **Purpose**: Displays the total salaries for each department.
   - **Details**: Iterates through the department totals and displays each department's total salary.
   - **Code**:
     ```cobol
     DISPLAY-DEPARTMENT-TOTALS.
         DISPLAY "Department Salary Totals".
         DISPLAY "-----------------------------".
         PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
             IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
                 DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
                 DISPLAY "Total Salary: $" TOTAL-SALARY(DEPARTMENT-INDEX)
                 DISPLAY "-----------------------------"
             END-IF
         END-PERFORM.
     ```

### Relationship Among Subroutines

1. **MAIN-PROCEDURE**:
   - Acts as the controller, calling each subroutine in sequence to perform the overall task of the payroll system.

2. **INITIALIZE-EMPLOYEES**:
   - Sets up the initial data required for further processing.

3. **SORT-EMPLOYEES**:
   - Organizes the employee data to facilitate easier calculations and display.

4. **CALCULATE-NET-SALARIES**:
   - Uses the sorted data to compute net salaries, which are necessary for department totals and display.

5. **CALCULATE-DEPARTMENT-TOTALS**:
   - Aggregates the net salaries calculated in the previous step to provide department-wise totals.

6. **DISPLAY-EMPLOYEES**:
   - Uses the sorted and calculated data to display individual employee details.

7. **DISPLAY-DEPARTMENT-TOTALS**:
   - Uses the aggregated department totals to display overall department salary information.

Each subroutine builds upon the results of the previous ones, ensuring a structured and logical flow of data processing and display in the payroll system.
</details>

## Step 3: Process of the COBOL code to a New Language

### Task 1: Ask GitHub Copilot to convert the code to Java

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I am looking to migrate a COBOL program to Java. I recognize that COBOL and Java have different concepts as programming languages. Therefore, I believe that I need to consider what steps I should take when converting and migrating source code. For example, I believe there are a number of considerations, such as how classes should be thought of in Java from the COBOL language, and how to migrate COBOL subroutines and copy clauses to Java. So, what are the steps to migrate from COBOL to Java?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Migrating a COBOL program to Java involves several steps to ensure that the concepts and structures in COBOL are appropriately mapped to Java. Here are the steps you should consider:

### 1. Project Planning and Preparation

- **Inventory of Codebase**:
  - Identify and document all COBOL programs, copybooks, and external dependencies.

- **Define Scope and Goals**:
  - Determine the scope of the migration and set goals (e.g., performance improvement, maintainability).

### 2. Mapping COBOL Concepts to Java

- **Program Structure**:
  - Map each COBOL program to a Java class. The `IDENTIFICATION DIVISION` in COBOL corresponds to the class declaration in Java.

- **Data Definitions**:
  - Convert data items defined in the `DATA DIVISION` to Java fields. Map COBOL data types to Java data types.

- **Subroutines**:
  - Convert COBOL subroutines (e.g., `PERFORM` and `CALL` statements) to Java methods. Treat subroutine arguments as method parameters.

- **COPYBOOK**:
  - Convert COBOL copybooks to reusable Java classes or interfaces.

### 3. Implementation of Migration

- **Create Classes**:
  - Create Java classes corresponding to each COBOL program. Name the classes based on the program names.

- **Define Fields**:
  - Define Java fields for COBOL data items. Perform data type conversions (e.g., `PIC 9(3)` to `int`).

- **Implement Methods**:
  - Implement Java methods for COBOL subroutines. Name the methods based on the subroutine names.

- **External Program Calls**:
  - Convert COBOL external program calls (`CALL` statements) to Java method calls.

### 4. Testing and Validation

- **Create Unit Tests**:
  - Develop unit tests for each Java class and method to ensure correct functionality.

- **Perform Integration Testing**:
  - Conduct integration testing to verify that the entire migrated Java codebase works correctly.

### 5. Optimization and Refactoring

- **Optimize Code**:
  - Optimize the migrated Java code for performance improvements.

- **Refactor Code**:
  - Refactor the code to improve readability and maintainability.

### Example of Converting COBOL Subroutine to Java

#### COBOL Subroutine
```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```

#### Java Method
```java
public class PayrollSystem {
    private Employee[] employees = new Employee[5];

    public void initializeEmployees() {
        employees[0] = new Employee("E001", "Alice Johnson", "HR", 70000.00);
        // Initialize other employees
    }
}

class Employee {
    private String id;
    private String name;
    private String department;
    private double grossSalary;

    public Employee(String id, String name, String department, double grossSalary) {
        this.id = id;
        this.name = name;
        this.department = department;
        this.grossSalary = grossSalary;
    }

    // Getters and setters
}
```

### Detailed Steps

1. **Inventory of Codebase**:
   - Collect all COBOL source files, copybooks, and related scripts.
   - Document the size and structure of the codebase.

2. **Define Scope and Goals**:
   - Identify which parts of the COBOL codebase will be migrated.
   - Set clear goals for the migration, such as improving performance or maintainability.

3. **Mapping COBOL Concepts to Java**:
   - **Program Structure**: Each COBOL program becomes a Java class.
   - **Data Definitions**: Convert COBOL data items to Java fields.
   - **Subroutines**: Convert COBOL subroutines to Java methods.
   - **COPYBOOK**: Convert copybooks to reusable Java classes or interfaces.

4. **Implementation of Migration**:
   - **Create Classes**: Create Java classes for each COBOL program.
   - **Define Fields**: Define Java fields for COBOL data items.
   - **Implement Methods**: Implement Java methods for COBOL subroutines.
   - **External Program Calls**: Convert external program calls to Java method calls.

5. **Testing and Validation**:
   - **Create Unit Tests**: Develop unit tests for each Java class and method.
   - **Perform Integration Testing**: Conduct integration testing to verify the entire migrated codebase.

6. **Optimization and Refactoring**:
   - **Optimize Code**: Optimize the Java code for performance.
   - **Refactor Code**: Refactor the code to improve readability and maintainability.

### Conclusion

Migrating from COBOL to Java requires careful planning and execution. By mapping COBOL concepts to Java, creating corresponding classes and methods, and thoroughly testing the migrated code, you can ensure a successful migration. Each step should be carefully documented and validated to maintain the integrity and functionality of the original COBOL program.
</details>

### Task 2: The use of GitHub Copilot in the conversion process

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I have reviewed the COBOL to Java migration steps. Now tell us how GitHub Copilot can help us in each of these steps.
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>
GitHub Copilot can assist in various ways during each step of the COBOL to Java migration process. Here’s how it can help:

### 1. Project Planning and Preparation

- **Inventory of Codebase**:
  - GitHub Copilot can generate scripts to list COBOL files and calculate code metrics such as lines of code and file sizes.

  ```python
  import os

  def list_cobol_files(directory):
      cobol_files = []
      for root, _, files in os.walk(directory):
          for file in files:
              if file.endswith(('.cbl', '.cob')):
                  cobol_files.append(os.path.join(root, file))
      return cobol_files

  directory_path = '/path/to/cobol/project'
  cobol_files = list_cobol_files(directory_path)
  for file in cobol_files:
      print(file)
  ```

- **Define Scope and Goals**:
  - GitHub Copilot can help generate a migration plan document template.

  ```markdown
  # Migration Plan

  ## Scope
  - List of COBOL programs to be migrated
  - Dependencies and external systems

  ## Goals
  - Improve performance
  - Enhance maintainability
  - Reduce technical debt

  ## Timeline
  - Phase 1: Inventory and Analysis
  - Phase 2: Code Conversion
  - Phase 3: Testing and Validation
  - Phase 4: Optimization and Refactoring
  ```

### 2. Mapping COBOL Concepts to Java

- **Program Structure**:
  - GitHub Copilot can generate templates for Java classes corresponding to COBOL programs.

  ```java
  public class PayrollSystem {
      // Fields and methods will be added here
  }
  ```

- **Data Definitions**:
  - GitHub Copilot can convert COBOL data items to Java fields.

  ```java
  public class Employee {
      private String id;
      private String name;
      private String department;
      private double grossSalary;

      // Constructor, getters, and setters
  }
  ```

- **Subroutines**:
  - GitHub Copilot can convert COBOL subroutines to Java methods.

  ```java
  public void initializeEmployees() {
      employees[0] = new Employee("E001", "Alice Johnson", "HR", 70000.00);
      // Initialize other employees
  }
  ```

- **COPYBOOK**:
  - GitHub Copilot can convert COBOL copybooks to reusable Java classes or interfaces.

  ```java
  public class EmployeeData {
      // Fields and methods for reusable data structures
  }
  ```

### 3. Implementation of Migration

- **Create Classes**:
  - GitHub Copilot can generate Java classes for each COBOL program.

  ```java
  public class PayrollSystem {
      private Employee[] employees = new Employee[5];

      // Methods will be added here
  }
  ```

- **Define Fields**:
  - GitHub Copilot can convert COBOL data items to Java fields.

  ```java
  private int employeeIndex;
  private double taxRate = 0.20;
  private double bonusRate = 0.10;
  private double deductionRate = 0.05;
  ```

- **Implement Methods**:
  - GitHub Copilot can convert COBOL subroutines to Java methods.

  ```java
  public void calculateNetSalaries() {
      for (Employee employee : employees) {
          double bonus = calculateBonus(employee.getGrossSalary());
          double netSalary = employee.getGrossSalary() + bonus - (employee.getGrossSalary() * taxRate) - (employee.getGrossSalary() * deductionRate);
          employee.setNetSalary(netSalary);
      }
  }

  private double calculateBonus(double grossSalary) {
      return grossSalary * bonusRate;
  }
  ```

- **External Program Calls**:
  - GitHub Copilot can convert COBOL external program calls to Java method calls.

  ```java
  public void callCalculateBonus(double grossSalary, double[] bonus) {
      bonus[0] = calculateBonus(grossSalary);
  }
  ```

### 4. Testing and Validation

- **Create Unit Tests**:
  - GitHub Copilot can generate unit tests for each Java class and method.

  ```java
  @Test
  public void testInitializeEmployees() {
      PayrollSystem payrollSystem = new PayrollSystem();
      payrollSystem.initializeEmployees();
      assertEquals("E001", payrollSystem.getEmployees()[0].getId());
  }
  ```

- **Perform Integration Testing**:
  - GitHub Copilot can generate templates for integration tests.

  ```java
  @Test
  public void testPayrollSystemIntegration() {
      PayrollSystem payrollSystem = new PayrollSystem();
      payrollSystem.initializeEmployees();
      payrollSystem.calculateNetSalaries();
      payrollSystem.calculateDepartmentTotals();
      payrollSystem.displayEmployees();
      payrollSystem.displayDepartmentTotals();
  }
  ```

### 5. Optimization and Refactoring

- **Optimize Code**:
  - GitHub Copilot can suggest code optimizations to improve performance.

  ```java
  public void optimizeNetSalaryCalculation() {
      for (Employee employee : employees) {
          double grossSalary = employee.getGrossSalary();
          double bonus = calculateBonus(grossSalary);
          double netSalary = grossSalary + bonus - (grossSalary * taxRate) - (grossSalary * deductionRate);
          employee.setNetSalary(netSalary);
      }
  }
  ```

- **Refactor Code**:
  - GitHub Copilot can suggest refactoring to improve code readability and maintainability.

  ```java
  public class Employee {
      private String id;
      private String name;
      private String department;
      private double grossSalary;
      private double netSalary;

      // Constructor, getters, and setters
  }
  ```

### Conclusion

GitHub Copilot can assist in each step of the COBOL to Java migration process by generating code, suggesting optimizations, refactoring, and creating tests. This helps streamline the migration project and ensures a smooth transition from COBOL to Java.
</details>

### Task 3: Ask GitHub Copilot to convert the code to Python

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I recognize that Java, Python and COBOL are different programming models. Therefore, I believe there are different considerations related to the GAP between Python and COBOL. What steps should I take to migrate from COBOL to Python? What are the steps to migrate from COBOL to Python?
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>
Migrating a COBOL program to Python involves several steps to ensure a smooth transition while maintaining the functionality of the original application. Here are the steps you should take:

### 1. **Understand the Existing COBOL Codebase**
   - **Inventory the Codebase**: Identify all COBOL programs, scripts, and related files.
   - **Document Functionality**: Understand and document the functionality of each COBOL program and subroutine.
   - **Identify Dependencies**: Identify internal and external dependencies, such as called programs and data sources.

### 2. **Plan the Migration**
   - **Define Scope**: Determine which parts of the COBOL codebase will be migrated.
   - **Set Objectives**: Define the goals of the migration (e.g., performance improvement, maintainability).
   - **Create a Roadmap**: Develop a detailed migration plan with timelines and milestones.

### 3. **Design the Python Architecture**
   - **Module Design**: Identify modules and functions that correspond to COBOL data structures and subroutines.
   - **Package Structure**: Organize the Python code into packages for better modularity.
   - **Error Handling**: Plan for exception handling in Python, which is different from COBOL's error handling.

### 4. **Migrate Data Structures**
   - **COBOL Records to Python Classes**: Convert COBOL record definitions to Python classes with appropriate attributes.
   - **Data Types**: Map COBOL data types to Python data types (e.g., `PIC X` to `str`, `PIC 9` to `int` or `float`).

### 5. **Migrate Subroutines**
   - **COBOL Subroutines to Python Functions**: Convert COBOL subroutines to Python functions.
   - **Function Parameters**: Use function parameters to pass data, similar to COBOL's `USING` clause.
   - **Control Structures**: Translate COBOL control structures (e.g., `PERFORM`, `IF`, `EVALUATE`) to Python control structures (`for`, `if`, `elif`).

### 6. **Implement Business Logic**
   - **Business Rules**: Ensure that all business rules and logic are correctly implemented in Python.
   - **Calculations and Algorithms**: Translate COBOL calculations and algorithms to Python.

### 7. **Handle Data Access**
   - **File I/O**: Convert COBOL file handling to Python file I/O or database access.
   - **Database Access**: Use libraries like `sqlite3`, `SQLAlchemy`, or `pandas` for database interactions.

### 8. **Testing and Validation**
   - **Unit Tests**: Write unit tests for individual Python classes and functions.
   - **Integration Tests**: Ensure that the integrated Python application works as expected.
   - **User Acceptance Testing**: Validate the migrated application with end-users to ensure it meets their requirements.

### 9. **Optimize and Refactor**
   - **Performance Tuning**: Optimize the Python code for performance.
   - **Code Refactoring**: Refactor the code to improve readability and maintainability.

### 10. **Deployment and Maintenance**
   - **Deployment Plan**: Develop a plan for deploying the Python application.
   - **Documentation**: Document the new Python codebase and provide training for the development team.
   - **Ongoing Maintenance**: Plan for ongoing maintenance and support of the Python application.

### Example: Migrating a COBOL Subroutine to Python

#### COBOL Subroutine
```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```

#### Python Function
```python
class Employee:
    def __init__(self, emp_id, name, department, gross_salary):
        self.emp_id = emp_id
        self.name = name
        self.department = department
        self.gross_salary = gross_salary

def initialize_employees():
    employees = []
    employees.append(Employee("E001", "Alice Johnson", "HR", 70000.00))
    employees.append(Employee("E002", "Bob Smith", "IT", 85000.00))
    employees.append(Employee("E003", "Charlie Brown", "Finance", 60000.00))
    employees.append(Employee("E004", "David Wilson", "Marketing", 75000.00))
    employees.append(Employee("E005", "Eve Davis", "HR", 75000.00))
    return employees

# Example usage
employees = initialize_employees()
for emp in employees:
    print(f"ID: {emp.emp_id}, Name: {emp.name}, Department: {emp.department}, Gross Salary: {emp.gross_salary}")
```

By following these steps, you can systematically migrate your COBOL program to Python, ensuring that the new application is robust, maintainable, and meets the original requirements.
</details>

## Step 4: Create a Design Document

### Task 1: Ask GitHub Copilot to create a design document template

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I would like to create a design document for our COBOL program. The design document should include the following sections, if possible:
   - Introduction
   - System Overview
   - Functional Requirements
   - Non-Functional Requirements
   - System Architecture
   - Data Model
   - User Interface Design
   - Error Handling and Logging
   - Testing Strategy
   - Deployment Plan

   Please provide a template for this design document.
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>

### Design Document Template for COBOL Program

#### 1. Introduction
   - **Purpose**: Describe the purpose of the document and the system it covers.
   - **Scope**: Define the scope of the system and what is included in the document.
   - **Audience**: Identify the intended audience for the document.

#### 2. System Overview
   - **System Description**: Provide a high-level description of the system and its main functionalities.
   - **Objectives**: Outline the main objectives and goals of the system.

#### 3. Functional Requirements
   - **Employee Data Initialization**: Describe the process of initializing employee data.
   - **Sorting Employees**: Explain how employees are sorted by their IDs.
   - **Net Salary Calculation**: Detail the calculation of net salaries, including bonuses, deductions, and tax deductions.
   - **Department Totals Calculation**: Describe how total salaries for each department are calculated.
   - **Displaying Information**: Explain how employee payroll information and department salary totals are displayed.

#### 4. Non-Functional Requirements
   - **Performance**: Define performance requirements, such as response time and throughput.
   - **Scalability**: Describe how the system can scale to handle increased load.
   - **Maintainability**: Outline requirements for maintainability, including code readability and documentation.
   - **Reliability**: Define reliability requirements, such as uptime and error rates.

#### 5. System Architecture
   - **High-Level Architecture**: Provide a high-level diagram and description of the system architecture.
   - **Components**: Describe the main components of the system and their interactions.

#### 6. Data Model
   - **Data Structures**: Define the data structures used in the system, including employee and department tables.
   - **Data Flow**: Describe how data flows through the system, from initialization to display.

#### 7. User Interface Design
   - **Console Output**: Describe the console output for displaying employee payroll information and department salary totals.
   - **User Interaction**: Explain how users interact with the system, if applicable.

#### 8. Error Handling and Logging
   - **Error Handling**: Describe how errors are handled in the system, including input validation and exception handling.
   - **Logging**: Outline the logging strategy for capturing system events and errors.

#### 9. Testing Strategy
   - **Unit Testing**: Describe the approach for unit testing individual components.
   - **Integration Testing**: Explain how integration testing will be performed to ensure components work together.
   - **User Acceptance Testing**: Outline the process for user acceptance testing to validate the system with end-users.

#### 10. Deployment Plan
   - **Deployment Steps**: Provide a step-by-step plan for deploying the system.
   - **Environment Setup**: Describe the environment setup required for deployment, including hardware and software requirements.
   - **Rollback Plan**: Outline a rollback plan in case of deployment issues.

This template provides a comprehensive structure for documenting the design of your COBOL program, ensuring that all key aspects are covered and clearly defined.
</details>

### Task 2: Ask GitHub Copilot to create a design document

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   Based on the design doc template, plase make a desing doc of #file:main.cobc
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>

### Design Document for `main.cobc-1`

#### 1. Introduction
   - **Purpose**: This document provides a detailed design for the COBOL payroll system, outlining its functionalities, architecture, and deployment plan.
   - **Scope**: The document covers the initialization, sorting, calculation, and display of employee payroll information and department salary totals.
   - **Audience**: This document is intended for developers, testers, and system administrators involved in the development and maintenance of the payroll system.

#### 2. System Overview
   - **System Description**: The payroll system initializes employee data, sorts employees, calculates net salaries, calculates department totals, and displays the results.
   - **Objectives**: The main objectives are to automate payroll processing, ensure accurate salary calculations, and provide clear payroll information.

#### 3. Functional Requirements
   - **Employee Data Initialization**: Initialize employee data with hardcoded values.
   - **Sorting Employees**: Sort employees by their IDs using a bubble sort algorithm.
   - **Net Salary Calculation**: Calculate net salaries by computing bonuses, deductions, and tax deductions.
   - **Department Totals Calculation**: Calculate total salaries for each department by aggregating net salaries.
   - **Displaying Information**: Display employee payroll information and department salary totals on the console.

#### 4. Non-Functional Requirements
   - **Performance**: The system should process payroll for up to 1000 employees within 5 seconds.
   - **Scalability**: The system should be able to scale to handle additional employees and departments.
   - **Maintainability**: The code should be well-documented and follow coding standards for readability and maintainability.
   - **Reliability**: The system should have an uptime of 99.9% and handle errors gracefully.

#### 5. System Architecture
   - **High-Level Architecture**: The system consists of a main program that calls subroutines for initialization, sorting, calculation, and display.
   - **Components**: The main components are the employee table, sorted employee table, department totals, and subroutines for each functionality.

#### 6. Data Model

##### Data Structures
- **EMPLOYEE-TABLE**
  - EMPLOYEE-ID: PIC X(5)
  - EMPLOYEE-NAME: PIC X(20)
  - DEPARTMENT: PIC X(10)
  - GROSS-SALARY: PIC 9(7)V99
  - BONUS: PIC 9(5)V99
  - DEDUCTIONS: PIC 9(5)V99
  - NET-SALARY: PIC 9(7)V99
  - TAX-DEDUCTION: PIC 9(5)V99

- **SORTED-EMPLOYEE-TABLE**
  - SORT-EMPLOYEE-ID: PIC X(5)
  - SORT-EMPLOYEE-NAME: PIC X(20)
  - SORT-DEPARTMENT: PIC X(10)
  - SORT-GROSS-SALARY: PIC 9(7)V99
  - SORT-BONUS: PIC 9(5)V99
  - SORT-DEDUCTIONS: PIC 9(5)V99
  - SORT-NET-SALARY: PIC 9(7)V99
  - SORT-TAX-DEDUCTION: PIC 9(5)V99

- **DEPARTMENT-TOTALS**
  - DEPT-NAME: PIC X(10)
  - TOTAL-SALARY: PIC 9(7)V99

- **Indexes and Rates**
  - EMPLOYEE-INDEX: PIC 9(3)
  - INNER-INDEX: PIC 9(3)
  - TAX-RATE: PIC 9V99 VALUE 0.20
  - BONUS-RATE: PIC 9V99 VALUE 0.10
  - DEDUCTION-RATE: PIC 9V99 VALUE 0.05
  - DEPARTMENT-INDEX: PIC 9(3)

- **Temporary Variables**
  - TEMP-ID: PIC X(5)
  - TEMP-NAME: PIC X(20)
  - TEMP-DEPARTMENT: PIC X(10)
  - TEMP-SALARY: PIC 9(7)V99

- **Linkage Section Variables**
  - LNK-GROSS-SALARY: PIC 9(7)V99
  - LNK-BONUS: PIC 9(5)V99

##### Data Flow
- **Initialization**: Employee data is initialized with hardcoded values.
- **Sorting**: Employee data is sorted by ID and stored in the sorted employee table.
- **Net Salary Calculation**: Net salaries are calculated using gross salary, bonus, deductions, and tax deductions.
- **Department Totals Calculation**: Total salaries for each department are calculated by aggregating net salaries.
- **Display**: Employee payroll information and department salary totals are displayed on the console.

#### 7. User Interface Design
   - **Console Output**: The system displays employee payroll information and department salary totals on the console.
   - **User Interaction**: Users interact with the system by running the program and viewing the console output.

#### 8. Error Handling and Logging
   - **Error Handling**: The system validates input data and handles errors during processing.
   - **Logging**: The system logs key events and errors to a log file for troubleshooting.

#### 9. Testing Strategy
   - **Unit Testing**: Each subroutine will be tested individually to ensure correct functionality.
   - **Integration Testing**: The system will be tested as a whole to ensure all components work together.
   - **User Acceptance Testing**: End-users will validate the system to ensure it meets their requirements.

#### 10. Deployment Plan
   - **Deployment Steps**:
     1. Compile the COBOL program.
     2. Deploy the executable to the target environment.
     3. Run initial tests to ensure the system is functioning correctly.
   - **Environment Setup**: The target environment should have a COBOL compiler and runtime installed.
   - **Rollback Plan**: In case of deployment issues, revert to the previous version of the system.

---

This design document provides a comprehensive structure for documenting the design of your COBOL program, ensuring that all key aspects are covered and clearly defined.

</details>

### Task 3: Ask GitHub Copilot to increase the readability of the code

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I would like to increase the readability of #file:main.cobc . Please comment on the design details.
   ```

<details>
Sure, let's add comments to the COBOL code to increase its readability and explain the design details.

### COBOL Code with Comments

```cobol
IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Define the maximum number of employees
       01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.

       * Define the employee table with details for each employee
       01  EMPLOYEE-TABLE.
           05  EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  EMPLOYEE-ID        PIC X(5).
               10  EMPLOYEE-NAME      PIC X(20).
               10  DEPARTMENT         PIC X(10).
               10  GROSS-SALARY       PIC 9(7)V99.
               10  BONUS              PIC 9(5)V99.
               10  DEDUCTIONS         PIC 9(5)V99.
               10  NET-SALARY         PIC 9(7)V99.
               10  TAX-DEDUCTION      PIC 9(5)V99.

       * Define the sorted employee table for storing sorted employee details
       01  SORTED-EMPLOYEE-TABLE.
           05  SORTED-EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  SORT-EMPLOYEE-ID   PIC X(5).
               10  SORT-EMPLOYEE-NAME PIC X(20).
               10  SORT-DEPARTMENT    PIC X(10).
               10  SORT-GROSS-SALARY  PIC 9(7)V99.
               10  SORT-BONUS         PIC 9(5)V99.
               10  SORT-DEDUCTIONS    PIC 9(5)V99.
               10  SORT-NET-SALARY    PIC 9(7)V99.
               10  SORT-TAX-DEDUCTION PIC 9(5)V99.

       * Define the department totals table for storing total salaries by department
       01  DEPARTMENT-TOTALS.
           05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
               10  DEPT-NAME          PIC X(10).
               10  TOTAL-SALARY       PIC 9(7)V99.

       * Define indexes and rates used in calculations
       01  EMPLOYEE-INDEX           PIC 9(3).
       01  INNER-INDEX              PIC 9(3).
       01  TAX-RATE                 PIC 9V99 VALUE 0.20.
       01  BONUS-RATE               PIC 9V99 VALUE 0.10.
       01  DEDUCTION-RATE           PIC 9V99 VALUE 0.05.
       01  DEPARTMENT-INDEX         PIC 9(3).

       * Define temporary variables used during sorting
       01  TEMP-ID                  PIC X(5).
       01  TEMP-NAME                PIC X(20).
       01  TEMP-DEPARTMENT          PIC X(10).
       01  TEMP-SALARY              PIC 9(7)V99.

       * Define linkage section variables for calling external programs
       01  LNK-GROSS-SALARY         PIC 9(7)V99.
       01  LNK-BONUS                PIC 9(5)V99.

       PROCEDURE DIVISION.

       * Main procedure to control the flow of the program
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-EMPLOYEES.
           PERFORM SORT-EMPLOYEES.
           PERFORM CALCULATE-NET-SALARIES.
           PERFORM CALCULATE-DEPARTMENT-TOTALS.
           PERFORM DISPLAY-EMPLOYEES.
           PERFORM DISPLAY-DEPARTMENT-TOTALS.
           STOP RUN.

       * Subroutine to initialize employee data
       INITIALIZE-EMPLOYEES.
           MOVE "E001" TO EMPLOYEE-ID(1).
           MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
           MOVE "HR" TO DEPARTMENT(1).
           MOVE 70000.00 TO GROSS-SALARY(1).

           MOVE "E002" TO EMPLOYEE-ID(2).
           MOVE "Bob Smith" TO EMPLOYEE-NAME(2).
           MOVE "IT" TO DEPARTMENT(2).
           MOVE 85000.00 TO GROSS-SALARY(2).

           MOVE "E003" TO EMPLOYEE-ID(3).
           MOVE "Charlie Brown" TO EMPLOYEE-NAME(3).
           MOVE "Finance" TO DEPARTMENT(3).
           MOVE 60000.00 TO GROSS-SALARY(3).

           MOVE "E004" TO EMPLOYEE-ID(4).
           MOVE "David Wilson" TO EMPLOYEE-NAME(4).
           MOVE "IT" TO DEPARTMENT(4).
           MOVE 95000.00 TO GROSS-SALARY(4).

           MOVE "E005" TO EMPLOYEE-ID(5).
           MOVE "Eve Davis" TO EMPLOYEE-NAME(5).
           MOVE "HR" TO DEPARTMENT(5).
           MOVE 75000.00 TO GROSS-SALARY(5).

       * Subroutine to sort employees by ID using bubble sort
       SORT-EMPLOYEES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               MOVE EMPLOYEE-NAME(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
               MOVE DEPARTMENT(EMPLOYEE-INDEX) TO SORT-DEPARTMENT(EMPLOYEE-INDEX)
               MOVE GROSS-SALARY(EMPLOYEE-INDEX) TO SORT-GROSS-SALARY(EMPLOYEE-INDEX)
           END-PERFORM.

           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX >= MAX-EMPLOYEES
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 UNTIL INNER-INDEX > MAX-EMPLOYEES - EMPLOYEE-INDEX
                   IF SORT-EMPLOYEE-ID(INNER-INDEX) > SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                       MOVE SORT-EMPLOYEE-ID(INNER-INDEX) TO TEMP-ID
                       MOVE SORT-EMPLOYEE-NAME(INNER-INDEX) TO TEMP-NAME
                       MOVE SORT-DEPARTMENT(INNER-INDEX) TO TEMP-DEPARTMENT
                       MOVE SORT-GROSS-SALARY(INNER-INDEX) TO TEMP-SALARY
                       MOVE SORT-EMPLOYEE-ID(INNER-INDEX + 1) TO SORT-EMPLOYEE-ID(INNER-INDEX)
                       MOVE SORT-EMPLOYEE-NAME(INNER-INDEX + 1) TO SORT-EMPLOYEE-NAME(INNER-INDEX)
                       MOVE SORT-DEPARTMENT(INNER-INDEX + 1) TO SORT-DEPARTMENT(INNER-INDEX)
                       MOVE SORT-GROSS-SALARY(INNER-INDEX + 1) TO SORT-GROSS-SALARY(INNER-INDEX)
                       MOVE TEMP-ID TO SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                       MOVE TEMP-NAME TO SORT-EMPLOYEE-NAME(INNER-INDEX + 1)
                       MOVE TEMP-DEPARTMENT TO SORT-DEPARTMENT(INNER-INDEX + 1)
                       MOVE TEMP-SALARY TO SORT-GROSS-SALARY(INNER-INDEX + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.

       * Subroutine to calculate net salaries for employees
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
               COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
               COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
           END-PERFORM.

       * Subroutine to calculate total salaries for each department
       CALCULATE-DEPARTMENT-TOTALS.
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
               MOVE 0 TO TOTAL-SALARY(DEPARTMENT-INDEX)
           END-PERFORM.

           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
                   IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES OR DEPT-NAME(DEPARTMENT-INDEX) = SORT-DEPARTMENT(EMPLOYEE-INDEX)
                       IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES
                           MOVE SORT-DEPARTMENT(EMPLOYEE-INDEX) TO DEPT-NAME(DEPARTMENT-INDEX)
                       END-IF
                       ADD SORT-NET-SALARY(EMPLOYEE-INDEX) TO TOTAL-SALARY(DEPARTMENT-INDEX)
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

       * Subroutine to display employee payroll information
       DISPLAY-EMPLOYEES.
           DISPLAY "Employee Payroll Information".
           DISPLAY "-----------------------------".
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               DISPLAY "Name: " SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
               DISPLAY "Department: " SORT-DEPARTMENT(EMPLOYEE-INDEX)
               DISPLAY "Gross Salary: $" SORT-GROSS-SALARY(EMPLOYEE-INDEX)
               DISPLAY "Bonus: $" SORT-BONUS(EMPLOYEE-INDEX)
               DISPLAY "Deductions: $" SORT-DEDUCTIONS(EMPLOYEE-INDEX)
               DISPLAY "Tax Deduction: $" SORT-TAX-DEDUCTION(EMPLOYEE-INDEX)
               DISPLAY "Net Salary: $" SORT-NET-SALARY(EMPLOYEE-INDEX)
               DISPLAY "-----------------------------"
           END-PERFORM.

       * Subroutine to display department salary totals
       DISPLAY-DEPARTMENT-TOTALS.
           DISPLAY "Department Salary Totals".
           DISPLAY "-----------------------------".
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
                   DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
                   DISPLAY "Total Salary: $" TOTAL-SALARY(DEPARTMENT-INDEX)
                   DISPLAY "-----------------------------"
               END-IF
           END-PERFORM.
```

### Summary of Comments
- **Data Division**: Comments explain the purpose of each data structure and variable.
- **Procedure Division**: Comments describe the purpose and functionality of each subroutine.
- **Main Procedure**: Comments outline the flow of the program, indicating the sequence of subroutine calls.

These comments should help increase the readability of the COBOL code and provide a clear understanding of the design details.

</details>

4. Click **Apply in Editor**
![Image](https://github.com/user-attachments/assets/6c1b88a8-92a4-4915-8357-91c2b9ee3631)

## Step 5: Convert the COBOL code to Java

### Task 1: Define the Class Conversion Criteria

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   What criteria should be considered for class design when reworking COBOL code into Java?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
When reworking COBOL code into Java, several criteria should be considered for class design to ensure the resulting code is maintainable, efficient, and leverages the object-oriented features of Java. Here are the key criteria:

### 1. **Encapsulation**
   - **Data Hiding**: Use private fields and provide public getter and setter methods to control access to the data.
   - **Modularity**: Group related data and behavior into classes to promote modularity and reusability.

### 2. **Single Responsibility Principle**
   - **Cohesion**: Ensure each class has a single responsibility and encapsulates related functionality.
   - **Separation of Concerns**: Separate different concerns into different classes (e.g., data representation, business logic, data access).

### 3. **Inheritance and Polymorphism**
   - **Hierarchy**: Use inheritance to create a class hierarchy where appropriate, promoting code reuse and extensibility.
   - **Interfaces and Abstract Classes**: Define interfaces or abstract classes to represent common behaviors and allow for polymorphic behavior.

### 4. **Composition over Inheritance**
   - **Composition**: Prefer composition over inheritance to achieve code reuse and flexibility. Use member objects to represent relationships between classes.

### 5. **Data Structures**
   - **Collections**: Use appropriate Java collections (e.g., `List`, `Map`, `Set`) to manage groups of objects.
   - **Data Types**: Map COBOL data types to appropriate Java data types (e.g., `String`, `int`, `double`).

### 6. **Error Handling**
   - **Exceptions**: Use Java's exception handling mechanism to manage errors and exceptional conditions.
   - **Validation**: Implement input validation and error checking to ensure data integrity.

### 7. **Scalability and Performance**
   - **Efficiency**: Design classes and methods to be efficient in terms of memory and processing time.
   - **Scalability**: Ensure the design can handle increased load and complexity without significant performance degradation.

### 8. **Maintainability**
   - **Readability**: Write clear and readable code with meaningful class and method names.
   - **Documentation**: Provide documentation and comments to explain the purpose and usage of classes and methods.

### 9. **Testability**
   - **Unit Testing**: Design classes and methods to be easily testable with unit tests.
   - **Mocking**: Use interfaces and dependency injection to facilitate mocking in tests.

</details>

### Task 2: Ask GitHub Copilot to define the class design

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   Based on the design doc #file:design-doc_main.md and conversion criteria #file:java-conversion-criteria.md , what classes do we need when we convert #file:main.cobc to Java code?
   Please tell the classes and the reason why you decided.
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Based on the design document and conversion criteria, we can identify the following classes needed when converting the COBOL code in main.cobc to Java:

### 1. **Employee Class**
   - **Reason**: Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction. This class encapsulates the data related to an employee and provides methods to access and modify the data.

### 2. **DepartmentTotal Class**
   - **Reason**: Represents the total salary for a department with attributes like department name and total salary. This class encapsulates the data related to department salary totals and provides methods to access and modify the data.

### 3. **PayrollSystem Class**
   - **Reason**: Contains methods to initialize employees, sort employees, calculate net salaries, calculate department totals, and display information. This class manages the overall payroll system and orchestrates the execution of various tasks.

### 4. **BonusCalculator Class**
   - **Reason**: Represents the functionality of the `CalculateBonus` COBOL program. This class contains a method to calculate the bonus based on the gross salary. It encapsulates the logic for bonus calculation and can be called from the `PayrollSystem` class.

### Summary
- **Employee Class**: Encapsulates employee data and provides methods to access and modify the data.
- **DepartmentTotal Class**: Encapsulates department total salary data.
- **PayrollSystem Class**: Manages the overall payroll system, including initializing employees, sorting, calculating salaries, and displaying information.
- **BonusCalculator Class**: Encapsulates the logic for calculating bonuses based on gross salary.

These classes ensure that the functionality of the original COBOL program is preserved while leveraging the object-oriented features of Java.

Similar code found with 1 license type
</details>

### Task 3: Java Class Detaild Design based on COBOL program

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   Based on the design doc #file:design-doc_main.md and conversion criteria #file:java-conversion-criteria.md , please provide the detailed class design of Employee Class.
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>

### Detailed Class Design for `Employee` Class

Based on the design document and conversion criteria, here is the detailed class design for the `Employee` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `Employee`
   - **Purpose**: Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.

#### 2. **Attributes**
   - **id**: `String`
     - Represents the employee's ID.
   - **name**: `String`
     - Represents the employee's name.
   - **department**: `String`
     - Represents the department the employee belongs to.
   - **grossSalary**: `double`
     - Represents the employee's gross salary.
   - **bonus**: `double`
     - Represents the employee's bonus.
   - **deductions**: `double`
     - Represents the employee's deductions.
   - **netSalary**: `double`
     - Represents the employee's net salary.
   - **taxDeduction**: `double`
     - Represents the employee's tax deduction.

#### 3. **Constructors**
   - **Employee(String id, String name, String department, double grossSalary)**
     - Initializes the employee with the given ID, name, department, and gross salary. Other attributes are initialized to default values.

#### 4. **Methods**
   - **Getters and Setters**
     - `String getId()`
     - `void setId(String id)`
     - `String getName()`
     - `void setName(String name)`
     - `String getDepartment()`
     - `void setDepartment(String department)`
     - `double getGrossSalary()`
     - `void setGrossSalary(double grossSalary)`
     - `double getBonus()`
     - `void setBonus(double bonus)`
     - `double getDeductions()`
     - `void setDeductions(double deductions)`
     - `double getNetSalary()`
     - `void setNetSalary(double netSalary)`
     - `double getTaxDeduction()`
     - `void setTaxDeduction(double taxDeduction)`

#### 5. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public getters and setters are provided to access and modify the attributes.

#### 6. **Maintainability**
   - The class is designed to be clear and readable with meaningful attribute names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 7. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `Employee` Class

```java
/**
 * Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.
 */
public class Employee {
    // Attributes
    private String id;
    private String name;
    private String department;
    private double grossSalary;
    private double bonus;
    private double deductions;
    private double netSalary;
    private double taxDeduction;

    /**
     * Initializes the employee with the given ID, name, department, and gross salary.
     * Other attributes are initialized to default values.
     *
     * @param id          the employee's ID
     * @param name        the employee's name
     * @param department  the department the employee belongs to
     * @param grossSalary the employee's gross salary
     */
    public Employee(String id, String name, String department, double grossSalary) {
        this.id = id;
        this.name = name;
        this.department = department;
        this.grossSalary = grossSalary;
        this.bonus = 0.0;
        this.deductions = 0.0;
        this.netSalary = 0.0;
        this.taxDeduction = 0.0;
    }

    // Getters and setters for all attributes

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDepartment() {
        return department;
    }

    public void setDepartment(String department) {
        this.department = department;
    }

    public double getGrossSalary() {
        return grossSalary;
    }

    public void setGrossSalary(double grossSalary) {
        this.grossSalary = grossSalary;
    }

    public double getBonus() {
        return bonus;
    }

    public void setBonus(double bonus) {
        this.bonus = bonus;
    }

    public double getDeductions() {
        return deductions;
    }

    public void setDeductions(double deductions) {
        this.deductions = deductions;
    }

    public double getNetSalary() {
        return netSalary;
    }

    public void setNetSalary(double netSalary) {
        this.netSalary = netSalary;
    }

    public double getTaxDeduction() {
        return taxDeduction;
    }

    public void setTaxDeduction(double taxDeduction) {
        this.taxDeduction = taxDeduction;
    }
}
```

### Summary
- **Encapsulation**: All attributes are private, and public getters and setters are provided.
- **Single Responsibility Principle**: The class focuses solely on representing employee data.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `Employee` class is robust, maintainable, and leverages the object-oriented features of Java.

</details>

### (Option) Task 4: Java Class Detaild Design based on COBOL program

Designing the detauiled class design other than `Employee` class.

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot same as `Employee` class.

<details>
<summary>3. GitHub Copilot Sample Response - `DepartmentTotal` class</summary>

### Detailed Class Design for DepartmentTotal Class

Based on the design document and conversion criteria, here is the detailed class design for the `DepartmentTotal` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `DepartmentTotal`
   - **Purpose**: Represents the total salary for a department with attributes like department name and total salary.

#### 2. **Attributes**
   - **departmentName**: `String`
     - Represents the name of the department.
   - **totalSalary**: `double`
     - Represents the total salary for the department.

#### 3. **Constructors**
   - **DepartmentTotal(String departmentName)**
     - Initializes the department with the given name and sets the total salary to 0.

#### 4. **Methods**
   - **Getters and Setters**
     - `String getDepartmentName()`
     - `void setDepartmentName(String departmentName)`
     - `double getTotalSalary()`
     - `void setTotalSalary(double totalSalary)`

#### 5. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public getters and setters are provided to access and modify the attributes.

#### 6. **Maintainability**
   - The class is designed to be clear and readable with meaningful attribute names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 7. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `DepartmentTotal` Class

```java
/**
 * Represents the total salary for a department with attributes like department name and total salary.
 */
public class DepartmentTotal {
    // Attributes
    private String departmentName;
    private double totalSalary;

    /**
     * Initializes the department with the given name and sets the total salary to 0.
     *
     * @param departmentName the name of the department
     */
    public DepartmentTotal(String departmentName) {
        this.departmentName = departmentName;
        this.totalSalary = 0;
    }

    // Getters and setters for all attributes

    public String getDepartmentName() {
        return departmentName;
    }

    public void setDepartmentName(String departmentName) {
        this.departmentName = departmentName;
    }

    public double getTotalSalary() {
        return totalSalary;
    }

    public void setTotalSalary(double totalSalary) {
        this.totalSalary = totalSalary;
    }
}
```

### Summary
- **Encapsulation**: All attributes are private, and public getters and setters are provided.
- **Single Responsibility Principle**: The class focuses solely on representing department total salary data.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `DepartmentTotal` class is robust, maintainable, and leverages the object-oriented features of Java.
</details>

<details>
<summary>4. GitHub Copilot Sample Response - PayrollSystem class</summary>

### Detailed Class Design for `PayrollSystem` Class

Based on the design document and conversion criteria, here is the detailed class design for the `PayrollSystem` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `PayrollSystem`
   - **Purpose**: Manages the overall payroll system, including initializing employees, sorting employees, calculating net salaries, calculating department totals, and displaying information.

#### 2. **Attributes**
   - **MAX_EMPLOYEES**: `static final int`
     - Represents the maximum number of employees.
   - **TAX_RATE**: `static final double`
     - Represents the tax rate.
   - **BONUS_RATE**: `static final double`
     - Represents the bonus rate.
   - **DEDUCTION_RATE**: `static final double`
     - Represents the deduction rate.
   - **employees**: `List<Employee>`
     - Represents the list of employees.
   - **departmentTotals**: `List<DepartmentTotal>`
     - Represents the list of department totals.

#### 3. **Methods**
   - **initializeEmployees()**
     - Initializes the employee data with hardcoded values.
   - **sortEmployees()**
     - Sorts the employees by their IDs.
   - **calculateNetSalaries()**
     - Calculates the net salaries for all employees.
   - **calculateDepartmentTotals()**
     - Calculates the total salaries for each department.
   - **displayEmployees()**
     - Displays the employee payroll information.
   - **displayDepartmentTotals()**
     - Displays the department salary totals.
   - **main(String[] args)**
     - The main method to run the payroll system.

#### 4. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public methods are provided to perform various operations on the payroll system.

#### 5. **Maintainability**
   - The class is designed to be clear and readable with meaningful method names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 6. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `PayrollSystem` Class

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Manages the overall payroll system, including initializing employees, sorting employees, calculating net salaries,
 * calculating department totals, and displaying information.
 */
public class PayrollSystem {
    // Constants
    private static final int MAX_EMPLOYEES = 5;
    private static final double TAX_RATE = 0.20;
    private static final double BONUS_RATE = 0.10;
    private static final double DEDUCTION_RATE = 0.05;

    // Attributes
    private List<Employee> employees = new ArrayList<>();
    private List<DepartmentTotal> departmentTotals = new ArrayList<>();

    /**
     * Initializes the employee data with hardcoded values.
     */
    public void initializeEmployees() {
        employees.add(new Employee("E001", "Alice Johnson", "HR", 70000.00));
        employees.add(new Employee("E002", "Bob Smith", "IT", 85000.00));
        employees.add(new Employee("E003", "Charlie Brown", "Finance", 60000.00));
        employees.add(new Employee("E004", "David Wilson", "IT", 95000.00));
        employees.add(new Employee("E005", "Eve Davis", "HR", 75000.00));
    }

    /**
     * Sorts the employees by their IDs.
     */
    public void sortEmployees() {
        Collections.sort(employees, Comparator.comparing(Employee::getId));
    }

    /**
     * Calculates the net salaries for all employees.
     */
    public void calculateNetSalaries() {
        BonusCalculator bonusCalculator = new BonusCalculator();
        for (Employee employee : employees) {
            double bonus = bonusCalculator.calculateBonus(employee.getGrossSalary());
            double deductions = employee.getGrossSalary() * DEDUCTION_RATE;
            double taxDeduction = employee.getGrossSalary() * TAX_RATE;
            double netSalary = employee.getGrossSalary() + bonus - taxDeduction - deductions;

            employee.setBonus(bonus);
            employee.setDeductions(deductions);
            employee.setTaxDeduction(taxDeduction);
            employee.setNetSalary(netSalary);
        }
    }

    /**
     * Calculates the total salaries for each department.
     */
    public void calculateDepartmentTotals() {
        for (Employee employee : employees) {
            DepartmentTotal departmentTotal = departmentTotals.stream()
                .filter(dt -> dt.getDepartmentName().equals(employee.getDepartment()))
                .findFirst()
                .orElseGet(() -> {
                    DepartmentTotal newDeptTotal = new DepartmentTotal(employee.getDepartment());
                    departmentTotals.add(newDeptTotal);
                    return newDeptTotal;
                });

            departmentTotal.setTotalSalary(departmentTotal.getTotalSalary() + employee.getNetSalary());
        }
    }

    /**
     * Displays the employee payroll information.
     */
    public void displayEmployees() {
        System.out.println("Employee Payroll Information");
        System.out.println("-----------------------------");
        for (Employee employee : employees) {
            System.out.println("Employee ID: " + employee.getId());
            System.out.println("Name: " + employee.getName());
            System.out.println("Department: " + employee.getDepartment());
            System.out.println("Gross Salary: $" + employee.getGrossSalary());
            System.out.println("Bonus: $" + employee.getBonus());
            System.out.println("Deductions: $" + employee.getDeductions());
            System.out.println("Tax Deduction: $" + employee.getTaxDeduction());
            System.out.println("Net Salary: $" + employee.getNetSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * Displays the department salary totals.
     */
    public void displayDepartmentTotals() {
        System.out.println("Department Salary Totals");
        System.out.println("-----------------------------");
        for (DepartmentTotal departmentTotal : departmentTotals) {
            System.out.println("Department: " + departmentTotal.getDepartmentName());
            System.out.println("Total Salary: $" + departmentTotal.getTotalSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * The main method to run the payroll system.
     *
     * @param args command-line arguments
     */
    public static void main(String[] args) {
        PayrollSystem payrollSystem = new PayrollSystem();
        payrollSystem.initializeEmployees();
        payrollSystem.sortEmployees();
        payrollSystem.calculateNetSalaries();
        payrollSystem.calculateDepartmentTotals();
        payrollSystem.displayEmployees();
        payrollSystem.displayDepartmentTotals();
    }
}
```

### Summary
- **Encapsulation**: All attributes are private, and public methods are provided to perform various operations.
- **Single Responsibility Principle**: The class focuses solely on managing the payroll system.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `PayrollSystem` class is robust, maintainable, and leverages the object-oriented features of Java.

</details>

<details>
<summary>5. GitHub Copilot Sample Response - BonusCalculator class</summary>

### Detailed Class Design for `BonusCalculator` Class

Based on the design document and conversion criteria, here is the detailed class design for the `BonusCalculator` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `BonusCalculator`
   - **Purpose**: Encapsulates the logic for calculating bonuses based on gross salary.

#### 2. **Attributes**
   - **BONUS_RATE**: `static final double`
     - Represents the bonus rate used for calculating bonuses.

#### 3. **Methods**
   - **calculateBonus(double grossSalary)**
     - Calculates the bonus based on the given gross salary.
     - **Parameters**: `grossSalary` (double) - The gross salary of the employee.
     - **Returns**: `double` - The calculated bonus.

#### 4. **Encapsulation**
   - The bonus rate is a private constant to ensure encapsulation.
   - The method `calculateBonus` is public to allow access to the bonus calculation logic.

#### 5. **Maintainability**
   - The class is designed to be clear and readable with meaningful method names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 6. **Testability**
   - The class is designed to be easily testable with unit tests.
   - The `calculateBonus` method can be tested individually to ensure correct functionality.

### Java Code for `BonusCalculator` Class

```java
/**
 * Encapsulates the logic for calculating bonuses based on gross salary.
 */
public class BonusCalculator {
    // Constants
    private static final double BONUS_RATE = 0.10;

    /**
     * Calculates the bonus based on the given gross salary.
     *
     * @param grossSalary the gross salary of the employee
     * @return the calculated bonus
     */
    public double calculateBonus(double grossSalary) {
        return grossSalary * BONUS_RATE;
    }
}
```

### Summary
- **Encapsulation**: The bonus rate is a private constant, and the `calculateBonus` method is public.
- **Single Responsibility Principle**: The class focuses solely on calculating bonuses.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `BonusCalculator` class is robust, maintainable, and leverages the object-oriented features of Java.
</details>

## Step 6: Generate Design Doc to Java

Based on the design doc, ask GitHub Copilot to generate the Java code.

### Task 1: Generate the Java code for `Employee` class

1. Open Copilot Edit with `Ctrl`+`Shift`+`I` or `Cmd`+`Shift`+`I`.

2. Ask GitHub Copilot for the following with `main.cobc` and `calculate_bonus.cobc`:

   ```plaintext
   Based on design doc #file:employee-class.md, please propose Employee class as Java under java directory.
   ```

3. Click `Accept` after you check the code.
![Image](https://github.com/user-attachments/assets/58fb683c-60b8-4d17-9e95-1cf3fe0f4f14)

### (Option) Task 2: Generate the Java code for `DepartmentTotal` class

1. Open Copilot Edit with `Ctrl`+`Shift`+`I` or `Cmd`+`Shift`+`I`.

2. Ask GitHub Copilot for the following with `main.cobc` and `calculate_bonus.cobc`:

   ```plaintext
   Based on design doc #file:departmenttotal-class.md, please propose DepartmentTotal class as Java under java directory.
   ```

### (Option) Task 3: Generate the Java code for `PayrollSystem` class

1. Open Copilot Edit with `Ctrl`+`Shift`+`I` or `Cmd`+`Shift`+`I`.

2. Ask GitHub Copilot for the following with `main.cobc` and `calculate_bonus.cobc`:

   ```plaintext
   Based on design doc #file:payrollsystem-class.md , please propose PayrollSystem class as Java under java directory.
   ```

### (Option) Task 4: Generate the Java code for `BonusCalculator` class

1. Open Copilot Edit with `Ctrl`+`Shift`+`I` or `Cmd`+`Shift`+`I`.

2. Ask GitHub Copilot for the following with `main.cobc` and `calculate_bonus.cobc`:

   ```plaintext
   Based on design doc #file:bonuscalculator-class.md , please propose BonusCalculator class as Java under java directory.
   ```

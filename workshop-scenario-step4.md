# Legacy Code Modernization Workshop

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

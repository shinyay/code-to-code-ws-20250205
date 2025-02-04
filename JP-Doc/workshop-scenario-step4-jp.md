# レガシーコードモダナイゼーションワークショップ

## ステップ4: 設計ドキュメントを作成する

### タスク1: GitHub Copilotに設計ドキュメントテンプレートを作成させる

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` でCopilotチャットを開きます。

2. GitHub Copilotに以下を依頼します:

```plaintext
COBOLプログラムの設計書を作成したいです。
まず、以下のようなセクションを含む設計書のテンプレートを提供してください。また、下記のような項目に加えてCOBOLプログラムの設計書として必要な項目があれば、それについても提供してください:

- はじめに
- システム概要
- 機能要件
- 非機能要件
- システムアーキテクチャ
- データモデル
- ユーザーインターフェース設計
- エラーハンドリングとログ
- テスト戦略
- デプロイメント計画

この設計ドキュメントのテンプレートを提供してください。
```

<details>
<summary>3. GitHub Copilotのサンプル応答</summary>

# COBOL Program Design Document

## はじめに
- **目的**: このドキュメントは、COBOLプログラムの設計を詳細に説明するためのものです。
- **対象読者**: 開発者、プロジェクトマネージャー、テスター、運用担当者
- **範囲**: プログラムの機能、非機能要件、システムアーキテクチャ、データモデル、ユーザーインターフェース設計、エラーハンドリング、テスト戦略、デプロイメント計画

## システム概要
- **システム名**:
- **システムの目的**:
- **システムの概要**:
- **主要な機能**:

## 機能要件
- **機能1**:
  - 説明:
  - 入力:
  - 出力:
- **機能2**:
  - 説明:
  - 入力:
  - 出力:

## 非機能要件
- **パフォーマンス要件**:
- **信頼性要件**:
- **可用性要件**:
- **セキュリティ要件**:
- **保守性要件**:

## システムアーキテクチャ
- **アーキテクチャ概要**:
- **コンポーネント図**:
- **データフロー図**:

## データモデル
- **データベース設計**:
  - テーブル定義:
  - 主キーと外部キー:
- **データ項目定義**:
  - 項目名:
  - データ型:
  - 説明:

## ユーザーインターフェース設計
- **画面設計**:
  - 画面1:
    - 説明:
    - 入力項目:
    - 出力項目:
- **操作フロー**:

## エラーハンドリングとログ
- **エラーハンドリングポリシー**:
- **エラーメッセージ**:
- **ログ出力**:
  - ログの種類:
  - ログフォーマット:

## テスト戦略
- **テスト計画**:
- **テストケース**:
  - テストケース1:
    - 説明:
    - 入力:
    - 期待される出力:
- **テスト環境**:

## デプロイメント計画
- **デプロイメント手順**:
- **環境設定**:
- **リリース管理**:

## 追加項目（COBOLプログラム特有の項目）
- **COBOLプログラム構造**:
  - IDENTIFICATION DIVISION:
  - ENVIRONMENT DIVISION:
  - DATA DIVISION:
  - PROCEDURE DIVISION:
- **COPYBOOKの使用**:
  - 使用するCOPYBOOKの一覧:
  - 各COPYBOOKの説明:
- **外部プログラムとの連携**:
  - 呼び出す外部プログラム:
  - 呼び出される外部プログラム:
- **バッチ処理の設計**:
  - バッチジョブのフロー:
  - スケジューリング:

## まとめ
- **今後の課題**:
- **参考文献**:
- **付録**:

</details>

4. `Insert into New File` をクリックします。

![image](https://github.com/user-attachments/assets/db19d43c-936e-416c-907c-ef522d40bfd3)

5. `designdoc-template.md` というファイル名を入力し、保存します。


### タスク2: GitHub Copilotに設計ドキュメントを作成させる

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` でCopilotチャットを開きます。

2. GitHub Copilotに以下を依頼します:

   ```plaintext
   設計書テンプレート #file:designdoc-template.md に基づいて、 #file:main.cobc  の設計書を作成してください。
   ```

<details>
<summary>3. GitHub Copilotのサンプル応答</summary>

### `main.cobc`の設計ドキュメント

#### 1. はじめに
   - **目的**: このドキュメントは、COBOL給与システムの詳細な設計を提供し、その機能、アーキテクチャ、およびデプロイメント計画を概説します。
   - **範囲**: このドキュメントは、従業員データの初期化、ソート、計算、および表示をカバーします。
   - **対象読者**: このドキュメントは、給与システムの開発および保守に関与する開発者、テスター、およびシステム管理者を対象としています。

#### 2. システム概要
   - **システム説明**: 給与システムは、従業員データを初期化し、従業員をソートし、純給与を計算し、部門ごとの合計を計算し、結果を表示します。
   - **目的**: 主な目的は、給与処理を自動化し、正確な給与計算を確保し、明確な給与情報を提供することです。

#### 3. 機能要件
   - **従業員データの初期化**: ハードコードされた値で従業員データを初期化します。
   - **従業員のソート**: バブルソートアルゴリズムを使用して従業員をIDでソートします。
   - **純給与の計算**: ボーナス、控除、および税控除を計算して純給与を計算します。
   - **部門ごとの合計計算**: 純給与を集計して各部門の給与合計を計算します。
   - **情報の表示**: コンソールに従業員の給与情報と部門ごとの給与合計を表示します。

#### 4. 非機能要件
   - **パフォーマンス**: システムは最大1000人の従業員の給与を5秒以内に処理する必要があります。
   - **スケーラビリティ**: システムは、追加の従業員および部門に対応できるようにスケーラブルである必要があります。
   - **保守性**: コードはよく文書化され、可読性と保守性のためにコーディング標準に従う必要があります。
   - **信頼性**: システムは99.9％の稼働時間を持ち、エラーを適切に処理する必要があります。

#### 5. システムアーキテクチャ
   - **高レベルアーキテクチャ**: システムは、初期化、ソート、計算、および表示のサブルーチンを呼び出すメインプログラムで構成されています。
   - **コンポーネント**: 主なコンポーネントは、従業員テーブル、ソートされた従業員テーブル、部門ごとの合計、および各機能のサブルーチンです。

#### 6. データモデル

##### データ構造
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

- **インデックスとレート**
  - EMPLOYEE-INDEX: PIC 9(3)
  - INNER-INDEX: PIC 9(3)
  - TAX-RATE: PIC 9V99 VALUE 0.20
  - BONUS-RATE: PIC 9V99 VALUE 0.10
  - DEDUCTION-RATE: PIC 9V99 VALUE 0.05
  - DEPARTMENT-INDEX: PIC 9(3)

- **一時変数**
  - TEMP-ID: PIC X(5)
  - TEMP-NAME: PIC X(20)
  - TEMP-DEPARTMENT: PIC X(10)
  - TEMP-SALARY: PIC 9(7)V99

- **リンクセクション変数**
  - LNK-GROSS-SALARY: PIC 9(7)V99
  - LNK-BONUS: PIC 9(5)V99

##### データフロー
- **初期化**: 従業員データはハードコードされた値で初期化されます。
- **ソート**: 従業員データはIDでソートされ、ソートされた従業員テーブルに格納されます。
- **純給与の計算**: 純給与は、基本給与、ボーナス、控除、および税控除を使用して計算されます。
- **部門ごとの合計計算**: 各部門の給与合計は、純給与を集計して計算されます。
- **表示**: コンソールに従業員の給与情報と部門ごとの給与合計が表示されます。

#### 7. ユーザーインターフェース設計
   - **コンソール出力**: システムは、コンソールに従業員の給与情報と部門ごとの給与合計を表示します。
   - **ユーザーインタラクション**: ユーザーはプログラムを実行し、コンソール出力を表示することでシステムと対話します。

#### 8. エラーハンドリングとログ
   - **エラーハンドリング**: システムは入力データを検証し、処理中のエラーを処理します。
   - **ログ**: システムは、トラブルシューティングのために主要なイベントとエラーをログファイルに記録します。

#### 9. テスト戦略
   - **単体テスト**: 各サブルーチンは、正しい機能を確保するために個別にテストされます。
   - **統合テスト**: システム全体がテストされ、すべてのコンポーネントが連携して動作することを確認します。
   - **ユーザー受け入れテスト**: エンドユーザーがシステムを検証し、要件を満たしていることを確認します。

#### 10. デプロイメント計画
   - **デプロイメント手順**:
     1. COBOLプログラムをコンパイルします。
     2. 実行可能ファイルをターゲット環境にデプロイします。
     3. システムが正しく機能していることを確認するために初期テストを実行します。
   - **環境設定**: ターゲット環境には、COBOLコンパイラとランタイムがインストールされている必要があります。
   - **ロールバック計画**: デプロイメントの問題が発生した場合、システムの以前のバージョンに戻します。

---

この設計ドキュメントは、COBOLプログラムの設計を文書化するための包括的な構造を提供し、すべての重要な側面がカバーされ、明確に定義されることを保証します。

</details>

4. `Insert into New File` をクリックします。

![image](https://github.com/user-attachments/assets/db19d43c-936e-416c-907c-ef522d40bfd3)

5. `designdoc-main.md` というファイル名を入力し、保存します。

### タスク3: GitHub Copilotにコードの可読性を向上させるよう依頼する

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` でCopilotチャットを開きます。

2. GitHub Copilotに以下を依頼します:

   ```plaintext
   設計書 #file:design-doc_main.md をもとに、 #file:main.cobc の可読性を向上させるようにコメントを挿入してください。
   ```

<details>
<summary>3. GitHub Copilotのサンプル応答</summary>

### COBOLコードの可読性向上

```cobol
IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * 従業員の最大数を定義
       01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.

       * 各従業員の詳細を含む従業員テーブルを定義
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

       * ソートされた従業員の詳細を格納するためのソート済み従業員テーブルを定義
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

       * 部門ごとの総給与を格納するための部門合計テーブルを定義
       01  DEPARTMENT-TOTALS.
           05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
               10  DEPT-NAME          PIC X(10).
               10  TOTAL-SALARY       PIC 9(7)V99.

       * 計算に使用されるインデックスとレートを定義
       01  EMPLOYEE-INDEX           PIC 9(3).
       01  INNER-INDEX              PIC 9(3).
       01  TAX-RATE                 PIC 9V99 VALUE 0.20.
       01  BONUS-RATE               PIC 9V99 VALUE 0.10.
       01  DEDUCTION-RATE           PIC 9V99 VALUE 0.05.
       01  DEPARTMENT-INDEX         PIC 9(3).

       * ソート中に使用される一時変数を定義
       01  TEMP-ID                  PIC X(5).
       01  TEMP-NAME                PIC X(20).
       01  TEMP-DEPARTMENT          PIC X(10).
       01  TEMP-SALARY              PIC 9(7)V99.

       * 外部プログラムを呼び出すためのリンクセクション変数を定義
       01  LNK-GROSS-SALARY         PIC 9(7)V99.
       01  LNK-BONUS                PIC 9(5)V99.

       PROCEDURE DIVISION.

       * プログラムのフローを制御するメイン手順
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-EMPLOYEES.
           PERFORM SORT-EMPLOYEES.
           PERFORM CALCULATE-NET-SALARIES.
           PERFORM CALCULATE-DEPARTMENT-TOTALS.
           PERFORM DISPLAY-EMPLOYEES.
           PERFORM DISPLAY-DEPARTMENT-TOTALS.
           STOP RUN.

       * 従業員データを初期化するサブルーチン
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

       * バブルソートを使用して従業員をIDでソートするサブルーチン
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

       * 従業員の純給与を計算するサブルーチン
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
               COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
               COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
           END-PERFORM.

       * 各部門の総給与を計算するサブルーチン
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

       * 従業員の給与情報を表示するサブルーチン
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

       * 部門ごとの給与合計を表示するサブルーチン
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

### コメントの概要
- **データ部門**: 各データ構造と変数の目的を説明するコメント。
- **手続き部門**: 各サブルーチンの目的と機能を説明するコメント。
- **メイン手順**: プログラムのフローを概説し、サブルーチン呼び出しの順序を示すコメント。

これらのコメントは、COBOLコードの可読性を高め、設計の詳細を明確に理解するのに役立ちます。

</details>

4. **エディタに適用** をクリックします。
![Image](https://github.com/user-attachments/assets/6c1b88a8-92a4-4915-8357-91c2b9ee3631)

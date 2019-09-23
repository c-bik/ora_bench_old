#include "dpi.h"
#include "embed/dpi.c"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

// config
#define NAME "scott"
#define PASS "regit"
#define CONN "(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=tcp)(HOST=127.0.0.1)(PORT=1521)))(CONNECT_DATA=(SERVICE_NAME=XE)))"
#define MAX_NUMBERS 100000

void mtracError(dpiContext *ctx, dpiErrorInfo err, unsigned line)
{
  if (ctx != NULL)
  dpiContext_getError(ctx, &err);

  printf(
    "%d: ERROR: %.*s (%s: %s)\n",
    line, err.messageLength, err.message, err.fnName, err.action
  );
}

int main(const int argCount, char *argVec[])
{
  dpiContext *ctx;
  dpiErrorInfo err;

  if (dpiContext_create(DPI_MAJOR_VERSION, DPI_MINOR_VERSION, &ctx, &err) < 0) {
    mtracError(NULL, err, __LINE__);
    exit(1);
  }

  dpiConn *conn;
  if (
    dpiConn_create(
      ctx, NAME, strlen(NAME), PASS, strlen(PASS), CONN, strlen(CONN),
      NULL, NULL, &conn
    ) < 0
  ) {
    mtracError(ctx, err, __LINE__);
    exit(1);
  }

  dpiData *arrayValue;
  dpiVar *stringColVar;
  const uint32_t maxArraySize = 10000;
  if (
    dpiConn_newVar(
      conn, DPI_ORACLE_TYPE_VARCHAR, DPI_NATIVE_TYPE_BYTES,
      maxArraySize, 16, 1, 0, NULL, &stringColVar, &arrayValue
    ) < 0
  ) {
    mtracError(ctx, err, __LINE__);
    exit(1);
  }

  dpiStmt *stmt;
  const char *sql = "insert into test (ITEM) values (:ITEM)";
  if (dpiConn_prepareStmt(conn, 0, sql, strlen(sql), NULL, 0, &stmt) < 0) {
    mtracError(ctx, err, __LINE__);
    exit(1);
  }

  if (dpiStmt_bindByName(stmt, "ITEM", strlen("ITEM"), stringColVar) < 0) {
    mtracError(ctx, err, __LINE__);
    exit(1);
  }

  char c[] = "41790000000\r\n";
  unsigned long count = 0;
  clock_t begin = clock();
  uint32_t idx = 0;
  do {
    c[11] = '\0';
    sprintf(c, "4179%07lu", count);
    count++;
    if (dpiVar_setFromBytes(stringColVar, idx, c, strlen(c)) < 0) {
        mtracError(ctx, err, __LINE__);
        exit(1);
    }
    idx++;
    if (idx >= maxArraySize) {
      idx = 0;
      if (dpiStmt_executeMany(stmt, DPI_MODE_EXEC_DEFAULT, maxArraySize) < 0) {
        mtracError(ctx, err, __LINE__);
        exit(1);
      }
    }

    if (count % 100000 == 0)
    printf("%lu\n", count);

  } while (count < MAX_NUMBERS);

  if (
    idx > 0 &&
    dpiStmt_executeMany(stmt, DPI_MODE_EXEC_DEFAULT, idx) < 0
  ) {
    mtracError(ctx, err, __LINE__);
    exit(1);
  }
  dpiConn_commit(conn);
  dpiVar_release(stringColVar);
  dpiConn_close(conn, DPI_MODE_CONN_CLOSE_DEFAULT, NULL, 0);
  dpiContext_destroy(ctx);

  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("%lu rows inserted in %f seconds\n", count, time_spent);

  return 0;
}

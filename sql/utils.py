import random


def accepted_msg(msg=None):
    return "Accepted", msg


def wrong_answer_msg(msg):
    return "Wrong Answer", msg


def runtime_error_msg(msg):
    return "Runtime Error", msg


def compile_error_msg(msg):
    return "Compile Error", msg


def time_limit_exceeded_msg(msg):
    return "Time Limit Exceeded", msg


def memory_limit_exceeded_msg(msg):
    return "Memory Limit Exceeded", msg


def system_error_msg(msg):
    return "System Error", msg


def create_random_database(conn, db_prefix):
    db_name = "%s_%d" % (db_prefix, random.randint(1000000000, 9999999999))
    cursor = conn.cursor()
    cursor.execute("CREATE DATABASE %s" % db_name)
    cursor.close()
    return db_name


def delete_database(conn, db_name):
    cursor = conn.cursor()
    cursor.execute("DROP DATABASE IF EXISTS %s" % db_name)
    conn.commit()
    cursor.close()


def exec_multi_query(conn, db_name, sql):
    cursor = conn.cursor(buffered=True)
    cursor.execute("USE %s" % db_name)
    res = cursor.execute(sql, multi=True)
    if res is not None:
        for _ in res:
            pass
    conn.commit()
    cursor.close()


def mysql_runtime_error_msg(e, scope):
    scope = "" if scope == "submission" else " [on: %s]" % scope
    return runtime_error_msg("%s%s" % (e, scope))


def mysql_compile_error_msg(e, scope):
    scope = "" if scope == "submission" else " [on: %s]" % scope
    return compile_error_msg("%s%s" % (e, scope))


def mysql_system_error_msg(e, scope):
    scope = "" if scope == "submission" else " [on: %s]" % scope
    return system_error_msg("%s%s" % (e, scope))


def mysql_time_limit_exceeded_msg(e, scope):
    scope = "" if scope == "submission" else " [on: %s]" % scope
    return time_limit_exceeded_msg("%s%s" % (e, scope))


def mysql_error_handler(exception, scope):
    if exception.errno in (1064, 1149):
        return mysql_compile_error_msg(exception, scope)
    elif exception.sqlstate is not None and exception.sqlstate[:2] in ("22", "23", "42"):
        return mysql_runtime_error_msg(exception, scope)
    elif exception.msg is not None and "max_statement_time" in exception.msg:
        return mysql_time_limit_exceeded_msg(exception, scope)
    elif exception.msg is not None and "executing multiple statements" in exception.msg:
        return mysql_runtime_error_msg("Executing multiple statements is not allowed.", scope)
    else:
        return mysql_system_error_msg(exception, scope)

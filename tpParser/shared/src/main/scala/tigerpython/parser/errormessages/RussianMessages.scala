/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package errormessages

import tigerpython.parser.errors.ErrorCode

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 23/02/2017
  * Updated by Tobias Kohn on 12/11/2019
  */
object RussianMessages extends ModifiableErrorMessages {
  import tigerpython.parser.errors.ErrorCode._

  protected def _getMessage(msg: ErrorCode.Value): String =
    msg match {
        case AND_CONNECTS_CMP_NOT_VARS => "'%s' связывает сравнения, а не переменные."
        case ARG_AFTER_VARARGS => "Запрещено передавать другие аргументы после распаковывающего аргумента."
        case AS_NOT_ALLOWED_HERE => "'as' здесь не разрешено / не поддерживается."
        case ASSIGNMENT_TO_RIGHT => "Целевой объект операции присваивания должен находиться слева."
        case USE_BREAK_INSTEAD_OF_RETURN => "Используйте 'break' вместо 'return' чтобы выйти из цикла."
        case BREAK_OUTSIDE_LOOP => "Инструкция '%s' не может находиться вне цикла."
        case CALL_NEEDS_PARENTHESES => "Чтобы вызвать функцию нужно добавить скобки, даже если они пустые."
        case CANNOT_APPLY_ASYNC => "'async' неприменимо в этой инструкции."
        case CANNOT_ASSIGN_TO_CALL => "Невозможно что-то присвоить вызову функции."
        case CANNOT_ASSIGN_TO_FUNCTION => "Невозможно что-то присвоить функции."
        case CANNOT_REDEFINE_NAME => "Имя '%s' уже занято."
        case CANNOT_TEST_TUPLE => "Тестируйте каждый элемент кортежа самостоятельно."
        case CANNOT_USE_KEYWORD_AS_NAME => "Вы не можете использовать ключевое слово '%s' в качестве имени."
        case CLASS_METHOD_WITHOUT_SELF => "Метод класса ожидает не меньше одного параметра."
        case COLON_EXPECTED => "Здесь необходимо двоеточие ':'."
        case CONDITION_CANNOT_BE_FULFILLED => "Условие бесполезно, так как никогда не выполняется."
        case CONDITION_ALWAYS_FULFILLED => "Условие бесполезно, так как всегда выполняется."
        case DECORATOR_NAME_CLASH => "Функция и её декоратор не могут называться одинаково: '%s'."
        case DECORATOR_NEEDS_CALLABLE => "Декораторы применимы только к функциям и классам."
        case DEFINITION_INSIDE_LOOP => "Определение '%s' не может находиться внутри цикла."
        case DOUBLE_ELSE => "У структуры '%s' не может быть больше одного 'else'."
        case DOUBLE_EQUAL_SIGN_EXPECTED => "Ожидается оператор сравнения '==', а не оператор присваивания."
        case DOUBLE_PARAMETER_NAMES => "Два параметра не могут называться одинаково: '%s'."
        case ELSE_MUST_BE_INDENTED => "Отступ '%s' должен равняться отступу 'if'."
        case ELSE_WITH_COMPARISON => "У 'else' не может быть сравнения."
        case ELSE_WITHOUT_IF => "Этому '%s' не соответствует никакой 'if'."
        case EMPTY_SUBSCRIPT => "Индекс массива не может быть пустым."
        case EXTRA_INDENTATION => "Лишний отступ."
        case EXTRA_LINEBREAK => "Кажется, присутствует лишний разрыв строки. Возможно, вы хотите его спрятать с помощью '\\'."
        case EXTRA_LEFT_BRACKET => "Лишная открывающая скобка: '%s'."
        case EXTRA_RIGHT_BRACKET => "Лишная закрывающая скобка: '%s'."
        case EXTRA_SPACE => "Лишний пробел."
        case EXTRA_SPACE_OR_MISSING_COMMA => "Лишний пробел или отсутствующая запятая."
        case EXTRA_TOKEN => "Лишняя лексема: '%s'."
        case FOREIGN_KEYWORD => "В Python нет ключевого слова '%s'."
        case FOREIGN_PRIVATE => "Недопустимая лексема '%s', используйте нижнее подчёркивание '_', чтобы пометить функцию как закрытую ('private')."
        case FOREIGN_STATEMENT => "В Python нет инструкции '%s'."
        case FOREIGN_SYNTAX => "Синтаксис %s недействителен в Python."
        case FOREIGN_TOKEN => "Недопустимая лексема '%s', используйте '%s'."
        case FOREIGN_VAR => "В Python не используется '%s' для определения переменных."
        case FOR_TARGET_NAME_REQUIRED => "Для цикла 'for' необходима переменная."
        case FUTURE_MUST_BE_FIRST => "Инструкция 'from __future__ import' должна быть первой инструкцией в модуле."
        case GENERATOR_CANNOT_RETURN_VALUE => "Генератор не может использовать 'return', чтобы вернуть значение. Возможно, вы имели в виду 'yield'."
        case GLOBAL_MUST_BE_FIRST => "Инструкции '%s' должны быть самыми первыми инструкциями в функции."
        case GLOBAL_OUTSIDE_FUNCTION => "Инструкция '%s' не может находиться вне функции."
        case IMPORT_INSIDE_LOOP => "Инструкция 'import' не может находиться в цикле."
        case INCOMPLETE_IMPORT => "Неполная инструкция import."
        case INCONSISTENT_INDENTATION => "Отступы не соответствуют друг другу."
        case INCONSISTENT_RETURNS => "Эта функция иногда возвращает значение, а иногда нет."
        case INDENTED_ELSE => "Лишние отступы: '%s'."
        case INFINITE_LOOP => "Бесконечный цикл."
        case INITIALIZATION_INSIDE_LOOP => "Инициализация не может находиться внутри цикла."
        case INVALID_ASSIGNMENT => "Невозможно присвоить что-то '%s'."
        case INVALID_FUNCTION_DEF => "Недопустимое определение функции."
        case INVALID_FUNCTION_DEF_ASSIGN => "Используйте ':' и 'return' вместо присваивания."
        case INVALID_GENERATOR_ARG => "Аргумент генератора/включения не может быть объединён с другими аргументами."
        case INVALID_INPUT_CHARACTER => "Вы ввели недействительный символ '%s'."
        case INVALID_KEY_VALUE_PAIR => "Пара 'ключ-значение' недействительна."
        case INVALID_NAME => "Недопустимое имя: '%s'."
        case INVALID_AUGASSIGN_TARGET => "Это выражение не может быть целью комбинированного присваивания."
        case INVALID_STRING_PREFIX => "Недействительное начало строки: '%s'."
        case INVALID_TOKEN_AT_START_OF_LINE => "Эта лексема недопустима в начале строки: '%s'."
        case METHOD_WITHOUT_SELF => "У метода должен быть параметр 'self'."
        case MISMATCHED_CLOSING_BRACKET => "Несоответствующие скобки: ожидается '%s', но найдено '%s'."
        case MISPLACED_ASSIGN => "Присваивание '%s' не может быть частью выражения."
        case MISSING_ASSIGNMENT => "Кажется, пропало присваивание."
        case MISSING_ASSIGNMENT_SOURCE => "Этой операции присваивания не хватает выражения-источника."
        case MISSING_BODY => "Отсутствует тело или отступ."
        case MISSING_COMMA => "Отсутствует запятая."
        case MISSING_COMPARISON => "Отсутствует сравнение."
        case MISSING_DOT => "Кажется, не хватает точки."
        case MISSING_LEFT_BRACKET => "Отсутствует открывающая скобка: '%s'."
        case MISSING_LEFT_PARENTHESIS => "Отсутствует открывающая круглая скобка '('."
        case MISSING_OPERATOR_OR_COMMA => "Отсутствует оператор или запятая."
        case MISSING_PARENTHESES => "Кажется, не хватает круглых скобок."
        case MISSING_RIGHT_BRACKET => "Отсутствует закрывающая скобка: '%s'."
        case MISSING_SPACE => "Отсутствует пробел."
        case MISSING_TOKEN => "Отсутствует '%s'."
        case MISSPELLED_KEYWORD => "Неправильно написанное ключевое слово: '%s' вместо '%s'."
        case MISSPELLED_NUMBER => "Кажется, в число закралась опечатка."
        case MISSPELLED_OPERATOR => "Неправильно написанный оператор: '%s' вместо '%s'."
        case MULTIPLE_VAR_ARGS => "Разрешено не более одного распаковывающего аргумента."
        case MULTIPLE_VAR_PARAMS => "Разрешено не более одного распаковывающего параметра."
        case NAME_EXPECTED => "Отсутствует имя."
        case NO_END_NEEDED => "В Python нет ключевого слова 'end'."
        case NO_PARAM_DEFAULT_ALLOWED => "У распаковывающего параметра не может быть значения по умолчанию."
        case NO_VIABLE_ALTERNATIVE => "Невозможно осмысленно интерпретировать код: '%s'."
        case NUMBER_NOT_SUBSCRIPTABLE => "У числа не может быть индекса массива."
        case PARAM_AFTER_KEYWORD_PARAM => "Ключевой распаковывающий аргумент должен идти последним."
        case PARAMS_REQUIRED => "Ожидается параметр/параметры, но найдено '%s'."
        case POS_ARG_AFTER_KEYWORD => "Позиционные параметры не могут идти после ключевых параметров."
        case POS_PARAM_AFTER_KEYWORD => "Параметры без значения по умолчанию не могут следовать за параметрами со значением по умолчанию или распаковывающими параметрами."
        case PRINT_DEST_EXPECTED => "После '>>' должен стоять действительный адрес вывода."
        case PRINT_IS_STATEMENT => "В Python 2.x 'print' является инструкцией и не может вызываться с именованными аргументами."
        case PRINT_NEEDS_PARENTHESES => "В Python 3.x 'print' является функцией и должно вызываться с круглыми скобками."
        case PYTHON_2_FEATURE_NOT_AVAILABLE => "Эта функция из Python 2.x недоступна."
        case PYTHON_3_FEATURE_NOT_AVAILABLE => "Эта функция из Python 3.x недоступна."
        case USE_RETURN_INSTEAD_OF_BREAK => "Используйте 'return' вместо 'break' чтобы выйти из функции."
        case RETURN_OUTSIDE_FUNCTION => "Инструкция 'return' не может находиться вне функции."
        case SINGLE_EQUAL_SIGN_EXPECTED => "Для операции присваивания используется (один!) знак '='."
        case SUPERFLUOUS_COMPARISON => "Сравнение с '%s' не требуется."
        case SWAPPED_TOKENS => "Кажется, лексемы перепутаны: '%s' и '%s'."
        case TOKEN_REQUIRED => "Ожидается'%s', но найдено '%s'."
        case TUPLE_NEEDS_PARENS => "Заключите кортеж в скобки."
        case UNEXPECTED_END_OF_INPUT => "Неожиданный конец строки или ввода."
        case UNEXPECTED_KEYWORD => "Ключевое слово '%s' не может здесь находиться."
        case UNMATCHED_BRACKET => "Отсутствует закрывающая скобка, соответствующая открывающей '%s'."
        case UNREACHABLE_CODE => "Недостижимый код: этот код не может быть достигнут и исполнен."
        case UNTERMINATED_STRING => "Строка не закончена. Возможно, не хватает закрывающих кавычек."
        case USE_AND_NOT_COMMA => "Сравнения соединены с помощью 'and' или 'or' вместо запятой."
        case USE_COMMA_NOT_AND => "Значения разделены запятыми вместо 'and'."
        case USE_ELIF_INSTEAD_OF_ELSE => "Используйте 'elif' вместо 'else'."
        case USE_ELIF_INSTEAD_OF_ELSE_IF => "Используйте 'elif' вместо 'else if'."
        case USE_EQ_INSTEAD_OF_NEQ => "Используйте '== %s' вместо '!= %s'."
        case USE_MOD_NOT_DIV => "Используйте '%%' вместо '/' чтобы проверить на делимость."
        case USE_NOT_INSTEAD_OF_FALSE => "Используйте 'not' вместо сравнения с '%s'."
        case USE_REPEAT_INSTEAD_OF_WHILE => "Используйте 'repeat' вместо 'while'."
        case USELESS_COMPUTATION => "Результат этого выражения нигде не используется."
        case USELESS_STATEMENT => "Бесполезная инструкция: она ничего не делает."
        case USELESS_STMT_USE_AUG_ASSIGN => "Бесполезная инструкция. Возможно, вы имели в виду '%s='?"
        case VARARG_AFTER_KEYWORD_ARG => "Распаковывающий параметр должен идти перед ключевым распаковывающим параметром."
        case VARARG_NOT_ALLOWED => "Распаковывающие параметры здесь запрещены."
        case WRONG_BRACKET => "Неподходящая скобка: ожидается '%s' вместо '%s'."
        case WRONG_TOKEN => "Неправильный символ '%s' вместо '%s'."
        case YIELD_OUTSIDE_FUNCTION => "Выражение 'yield' не может находиться вне функции."
        case _ => null
    }
}
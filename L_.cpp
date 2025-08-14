/*
初始的开发环境在linux,Windows10的编译没有问题
现在的if语句的语法有点问题，暂时用不了（我不会修）
代码经过AI整理
文件直接平铺在了仓库，没有目录结构，不要介意（我懒）
如果你愿意更新这个代码，就放开了干吧，
但一定要在main函数所在的文件开始处添加像上面这样交流用的注释
*/
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <iterator>
#include <variant>
#include <memory>
#include <optional>
#include <cstdlib>
using namespace std;

class ExecutionContext;
class HandlerRegistry;
class CustomInterpreter;
using ValueVariant = variant<int, float, string, bool>;

class Variable {
    ValueVariant value;
public:
    const auto& getVariant() const { return value; }
    
    template<typename T>
    void set(T val) { value = val; }

    template<typename T>
    T get() const { 
        if (holds_alternative<T>(value)) {
             return std::get<T>(value);
        } else {
            throw runtime_error("类型不匹配");
        }
    }

    string typeName() const {
        return visit([](auto&& arg) {
            using T = decay_t<decltype(arg)>;
            if constexpr (is_same_v<T, int>) return "int";
            else if constexpr (is_same_v<T, float>) return "float";
            else if constexpr (is_same_v<T, string>) return "string";
            else if constexpr (is_same_v<T, bool>) return "bool"; 
            else return "unknown";
        }, value);
    }
};


struct ExecutionContext {
    unordered_map<string, Variable> variables;
    ostream& output;
    istream& input;
    string currentLine;
    int lineNumber = 0;
    unordered_map<string, int> labels; // 存储标签名与行号的映射
    int currentLineIndex = 0;         // 当前执行的行索引

    ExecutionContext() : output(cout), input(cin) {}

    template<typename T>
    void setVariable(const string& name, T value) {
        variables[name].set(value);
    }

    optional<Variable> getVariable(const string& name) const {
        if (auto it = variables.find(name); it != variables.end())
            return it->second;
        return nullopt;
    }

    void reportError(const string& message) {
        cerr << "错误 [行" << lineNumber << "]: " << message << endl;
    }
};

// ================== 指令处理器系统 ==================
class InstructionHandler {
public:
    virtual ~InstructionHandler() = default;
    virtual bool canHandle(const string& line) const = 0;
    virtual void handle(const string& line, ExecutionContext& context) = 0;
    ValueVariant parseValue(const string& s);
};

// 标签处理器
class SignHandler : public InstructionHandler {
    HandlerRegistry& registry;

public:
    SignHandler(HandlerRegistry& registry) : registry(registry) {}

    bool canHandle(const string& line) const override {
        return line.find("sign_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        string labelName = line.substr(5);
        ctx.labels[labelName] = ctx.lineNumber; // 记录当前行号
    }
};

// ================== 处理器注册中心 ==================
class HandlerRegistry {
    vector<unique_ptr<InstructionHandler>> handlers;

public:
    HandlerRegistry() {}

    template<typename T, typename... Args>
    void registerHandler(Args&&... args) {
        handlers.push_back(make_unique<T>(forward<Args>(args)...));
    }

    InstructionHandler* findHandler(const string& line) {
        for (auto& handler : handlers) {
            if (handler->canHandle(line)) {
                return handler.get();
            }
        }
        return nullptr;
    }
};

// 跳转处理器
class JumpHandler : public InstructionHandler {
    HandlerRegistry& registry;

public:
    JumpHandler(HandlerRegistry& registry) : registry(registry) {}

    bool canHandle(const string& line) const override {
        return line.find("jump_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        string labelType = line.substr(5);
        
        // 处理条件跳转（示例：jump_if_xxx_label）
        if (labelType.find("if_") == 0) {
            size_t pos = labelType.find('_', 3);
            string varName = labelType.substr(3, pos-3);
            string targetLabel = labelType.substr(pos+1);

            if (auto var = ctx.getVariable(varName); var) {
                bool condition = var->get<int>(); // 假设条件变量为int类型
                if (condition && ctx.labels.count(targetLabel)) {
                    ctx.currentLineIndex = ctx.labels[targetLabel] - 2; // 调整行索引
                }
            }
        } 
        // 处理无条件跳转（示例：jump_loop1）
        else if (ctx.labels.count(labelType)) {
            ctx.currentLineIndex = ctx.labels[labelType] - 2; // 行号转索引
        } else {
            ctx.reportError("未定义的标签: " + labelType);
        }
    }
};

// 算术处理器
class ArithmeticHandler : public InstructionHandler {
public:
    bool canHandle(const string& line) const override {
        return line.find("calc_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        size_t base_pos = 5; // "calc_" 的长度
        
        // 解析结果变量名
        size_t bracket_pos = line.find('[', base_pos);
        if (bracket_pos == string::npos) {
            ctx.reportError("缺少操作符括号");
            return;
        }
        
        string result_var = line.substr(base_pos, bracket_pos - base_pos);
        result_var.erase(remove_if(result_var.begin(), result_var.end(), ::isspace), result_var.end());
        
        // 解析操作符
        size_t bracket_end = line.find(']', bracket_pos);
        if (bracket_end == string::npos) {
            ctx.reportError("操作符括号未闭合");
            return;
        }
        string op = line.substr(bracket_pos + 1, bracket_end - bracket_pos - 1);
        
        // 解析操作数
        size_t semicolon_pos = line.find(';', bracket_end);
        if (semicolon_pos == string::npos) {
            ctx.reportError("缺少操作数分隔符;");
            return;
        }
        
        string var1 = line.substr(bracket_end + 1, semicolon_pos - bracket_end - 1);
        string var2 = line.substr(semicolon_pos + 1);
        
        // 去除空白字符
        var1.erase(remove_if(var1.begin(), var1.end(), ::isspace), var1.end());
        var2.erase(remove_if(var2.begin(), var2.end(), ::isspace), var2.end());
        auto opt_var1 = ctx.getVariable(var1);
        auto opt_var2 = ctx.getVariable(var2);

        if (!opt_var1 || !opt_var2) {
            ctx.reportError("未定义的变量: " + (!opt_var1 ? var1 : var2));
            return;
        }

        try {
            // 自动类型转换逻辑
            auto get_value = [](const Variable& var) -> variant<int, float> {
                if (var.typeName() == "int") return var.get<int>();
                if (var.typeName() == "float") return var.get<float>();
                throw runtime_error("不支持的操作数类型");
            };

            auto val1 = get_value(*opt_var1);
            auto val2 = get_value(*opt_var2);

            // 计算结果
            visit([&](auto&& v1) {
                visit([&](auto&& v2) {
                    using T1 = decay_t<decltype(v1)>;
                    using T2 = decay_t<decltype(v2)>;
                    using ResultType = conditional_t<is_floating_point_v<T1> || is_floating_point_v<T2>, float, int>;

                    ResultType result = 0;
                    if (op == "+") result = v1 + v2;
                    else if (op == "-") result = v1 - v2;
                    else if (op == "*") result = v1 * v2;
                    else if (op == "/") {
                        if (v2 == 0) {
                            ctx.reportError("除数不能为零");
                            return;
                        }
                        result = v1 / v2;
                    } else {
                        ctx.reportError("无效的操作符: " + op);
                        return;
                    }

                    // 自动创建结果变量
                    ctx.setVariable(result_var, result);
                }, val2);
            }, val1);
            
        } catch (const exception& e) {
            ctx.reportError(string("运算错误: ") + e.what());
        }
    }
};

// 输出处理器
class OutputHandler : public InstructionHandler {
public:
    bool canHandle(const string& line) const override {
        return line.find("output_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        if (line[7] == '"') {
            size_t end_quote = line.find('"', 8);
            if (end_quote != string::npos) {
                ctx.output << line.substr(8, end_quote - 8) << endl;
            }
        } else {
            string varName = line.substr(7);
            if (auto var = ctx.getVariable(varName)) {
                visit([&](auto&& arg) {
                    // 特殊处理布尔值输出
                    if constexpr (is_same_v<decay_t<decltype(arg)>, bool>) {
                        ctx.output << (arg ? "true" : "false") << endl;
                    } else {
                        ctx.output << arg << endl;
                    }
                }, var->getVariant());
            } else {
                ctx.reportError("变量未定义: " + varName);
            }
        }
    }
};

// 定义变量处理器
template<typename T>
class DefineVarHandler : public InstructionHandler {
    string typePrefix;
    string typeName;

public:
    DefineVarHandler(string prefix, string type) 
        : typePrefix(move(prefix)), typeName(move(type)) {}

    bool canHandle(const string& line) const override {
        return line.find("define_" + typePrefix + "_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        size_t bracketStart = line.find('[');
        size_t bracketEnd = line.find(']');

        if (bracketStart == string::npos || bracketEnd == string::npos) {
            ctx.reportError("无效的变量定义语法");
            return;
        }

        try {
            string varName = line.substr(
                line.find("_", 7) + 1, 
                bracketStart - (line.find("_", 7) + 1)
            );

            string valueStr = line.substr(
                bracketStart + 1, 
                bracketEnd - bracketStart - 1
            );

            T value;
            if constexpr (is_same_v<T, int>) {
                value = stoi(valueStr);
            } else if constexpr (is_same_v<T, float>) {
                value = stof(valueStr);
            } else if constexpr (is_same_v<T, string>) {
                value = valueStr;
            } else if constexpr (is_same_v<T, bool>) { 
                transform(valueStr.begin(), valueStr.end(), valueStr.begin(), ::tolower);
                if (valueStr == "true" || valueStr == "1") {
                    value = true;
                } else if (valueStr == "false" || valueStr == "0") {
                    value = false;
                } else {
                    throw runtime_error("无效的布尔值");
                }
            }

            ctx.setVariable(varName, value);
        } catch (const exception& e) {
            ctx.reportError("变量定义错误: " + string(e.what()));
        }
    }
};

// 系统指令处理器
class SystemHandler : public InstructionHandler {
public:
    bool canHandle(const string& line) const override {
        return line.find("system_") == 0;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        // 解析命令
        string command = line.substr(7);
        command.erase(remove_if(command.begin(), command.end(), ::isspace), command.end());

        if (command.empty()) {
            ctx.reportError("system_指令缺少命令");
            return;
        }

        // 执行系统命令
        int result = system(command.c_str());

        if (result == -1) {
            ctx.reportError("system_指令执行失败: " + command);
        }
    }
};

// 如果指令处理器
class IfHandler : public InstructionHandler {
    HandlerRegistry& registry;

public:
    IfHandler(HandlerRegistry& registry) : registry(registry) {}

    bool canHandle(const string& line) const override {
        return line.find("if_(") == 0 && line.find(")t[") != string::npos;
    }

    void handle(const string& line, ExecutionContext& ctx) override {
        size_t expr_end = line.find(")t[");
        if (expr_end == string::npos) {
            ctx.reportError("if_指令格式错误");
            return;
        }

        string expression = line.substr(3, expr_end - 3);
        string statement = line.substr(expr_end + 3, line.size() - expr_end - 4);

        expression.erase(remove_if(expression.begin(), expression.end(), ::isspace), expression.end());
        statement.erase(remove_if(statement.begin(), statement.end(), ::isspace), statement.end());

        if (expression.empty() || statement.empty()) {
            ctx.reportError("if_指令缺少表达式或执行语句");
            return;
        }

        bool condition = evaluateExpression(expression, ctx);
        if (condition) {
            executeSingleLine(statement, ctx);
        }
    }

private:

    ValueVariant parseValue(const string& s, ExecutionContext& ctx) {
        // 新增带引号的字符串支持
        if (s.front() == '"' && s.back() == '"') {
            return s.substr(1, s.length() - 2);
        }
        // 检测布尔值
        if (s == "true") return true;
        if (s == "false") return false;
        // 检测数字
        try {
            if (s.find('.') != string::npos) {
                return stof(s);
            }
            return stoi(s);
        } catch (...) {
            return s; // 作为变量名回退
        }
    }
    bool evaluateExpression(const string& expression, ExecutionContext& ctx) {
        // 新的操作符检测逻辑
        vector<string> operators = {"==", "!=", "<=", ">=", "<", ">"};
        size_t op_pos = string::npos;
        string detected_op;

        // 寻找最长的匹配操作符
        for (const auto& op : operators) {
            size_t pos = expression.find(op);
            if (pos != string::npos) {
                // 确保操作符不在开头或结尾
                if (pos > 0 && pos + op.length() < expression.length()) {
                    detected_op = op;
                    op_pos = pos;
                    break;
                }
            }
        }

        if (op_pos == string::npos) {
            ctx.reportError("无效的表达式格式: " + expression);
            return false;
        }

        string var1 = expression.substr(0, op_pos);
        string var2 = expression.substr(op_pos + detected_op.length());
        string op = detected_op;

        // 处理变量名中的空格
        var1.erase(remove_if(var1.begin(), var1.end(), ::isspace), var1.end());
        var2.erase(remove_if(var2.begin(), var2.end(), ::isspace), var2.end());

        // 获取左侧值
        ValueVariant left;
        if (auto var = ctx.getVariable(var1); var) {
            left = var->getVariant();
        } else {
            left = parseValue(var1, ctx);
        }

        // 获取右侧值
        ValueVariant right;
        if (auto var = ctx.getVariable(var2); var) {
            right = var->getVariant();
        } else {
            right = parseValue(var2, ctx);
        }

        return compareValues(left, right, op, ctx);
    }

    bool compareValues(const ValueVariant& left,
                       const ValueVariant& right,
                       const string& op, 
                       ExecutionContext& ctx) {
        return visit([&](auto&& l) {
            return visit([&](auto&& r) {
                using L = decay_t<decltype(l)>;
                using R = decay_t<decltype(r)>;
                
                // 允许数字类型相互比较
                if constexpr (is_arithmetic_v<L> && is_arithmetic_v<R>) {
                    return compareValuesImpl(static_cast<double>(l), 
                                            static_cast<double>(r), op, ctx);
                }
                // 字符串比较
                else if constexpr (is_same_v<L, string> && is_same_v<R, string>) {
                    return compareValuesImpl(l, r, op, ctx);
                }
                // 布尔比较
                else if constexpr (is_same_v<L, bool> && is_same_v<R, bool>) {
                    return compareValuesImpl(l, r, op, ctx);
                }
                else {
                    ctx.reportError("类型不匹配: " + string(typeid(L).name()) + 
                                    " vs " + string(typeid(R).name()));
                    return false;
                }
            }, right);
        }, left);
    }
    using ValueVariant = std::variant<int, float, std::string, bool>;
    template<typename T1, typename T2>
    bool compareValuesImpl(T1 left, T2 right, const string& op, ExecutionContext& ctx) {
        if (op == "==") return left == right;
        else if (op == "!=") return left != right;
        else if (op == "<") return left < right;
        else if (op == ">") return left > right;
        else if (op == "<=") return left <= right;
        else if (op == ">=") return left >= right;
        else {
            ctx.reportError("不支持的操作符: " + op);
            return false;
        }
    }

    void executeSingleLine(const string& line, ExecutionContext& ctx) {
        ctx.currentLine = line;
        ctx.currentLineIndex--; // 使当前行重新执行
        if (auto handler = registry.findHandler(line); handler) {
            handler->handle(line, ctx);
        } else {
            ctx.reportError("未知指令: " + line);
        }
        ctx.currentLineIndex++; // 执行完语句后恢复索引
    }
};
// ================== 解释器核心 ==================
class CustomInterpreter {
    HandlerRegistry registry;
    ExecutionContext context;

    vector<string> preprocess(const string& program) {
        vector<string> lines;
        stringstream ss(program);
        string line;

        while (getline(ss, line, '\n')) {
            size_t comment_pos = line.find("//");
            if (comment_pos != string::npos) {
                line = line.substr(0, comment_pos);
            }
            line.erase(line.begin(), find_if(line.begin(), line.end(), [](int ch) {
                return !isspace(ch);
            }));
            line.erase(find_if(line.rbegin(), line.rend(), [](int ch) {
                return !isspace(ch);
            }).base(), line.end());

            if (!line.empty()) {
                lines.push_back(line);
            }
        }
        return lines;
    }

    void executeSingleLine(const string& line, ExecutionContext& ctx) {
        if (auto handler = registry.findHandler(line); handler) {
            handler->handle(line, ctx);
        } else {
            ctx.reportError("未知指令: " + line);
        }
    }

public:
CustomInterpreter() {
    registry.registerHandler<OutputHandler>();
    registry.registerHandler<DefineVarHandler<int>>("int", "integer");
    registry.registerHandler<DefineVarHandler<float>>("float", "float");
    registry.registerHandler<DefineVarHandler<string>>("string", "string");
    registry.registerHandler<DefineVarHandler<bool>>("bool", "boolean"); 
    registry.registerHandler<SignHandler>(registry);
    registry.registerHandler<JumpHandler>(registry);
    registry.registerHandler<ArithmeticHandler>();
    registry.registerHandler<SystemHandler>();
    registry.registerHandler<IfHandler>(registry);
}

    void execute(const string& program) {
        auto lines = preprocess(program);
        context.currentLineIndex = 0;

        while (context.currentLineIndex < lines.size()) {
            auto& line = lines[context.currentLineIndex];
            context.lineNumber = context.currentLineIndex + 1;

            if (auto handler = registry.findHandler(line); handler) {
                handler->handle(line, context);
            } else {
                context.reportError("未知指令: " + line);
            }

            context.currentLineIndex++; // 正常流程+1，跳转时会修改这个值
        }
    }

    HandlerRegistry& getRegistry() {
        return registry;
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cerr << "用法: " << argv[0] << " <文件名>" << endl;
        return 1;
    }

    ifstream file(argv[1]);
    if (!file) {
        cerr << "无法打开文件: " << argv[1] << endl;
        return 1;
    }

    string program((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    CustomInterpreter interpreter;
    interpreter.execute(program);

    return 0;
}

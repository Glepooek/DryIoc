/*
The MIT License (MIT)

Copyright (c) 2016 Maksim Volkau

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included AddOrUpdateServiceFactory
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

// ReSharper disable CoVariantArrayConversion

namespace FastExpressionCompiler
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;
    using System.Reflection.Emit;
    using System.Diagnostics;

    /// <summary>Compiles expression to delegate ~20 times faster than Expression.Compile.
    /// Partial to extend with your things when used as source file.</summary>
    // ReSharper disable once PartialTypeWithSinglePart
    public static partial class ExpressionCompiler
    {
        #region Obsolete APIs

        /// <summary>Obsolete: replaced by CompileFast extension method</summary>
        public static Func<T> Compile<T>(Expression<Func<T>> lambdaExpr) =>
            lambdaExpr.CompileFast<Func<T>>();

        /// <summary>Obsolete: replaced by CompileFast extension method</summary>
        public static TDelegate Compile<TDelegate>(LambdaExpression lambdaExpr)
            where TDelegate : class =>
            TryCompile<TDelegate>(lambdaExpr) ?? (TDelegate)(object)lambdaExpr.Compile();

        #endregion

        #region Expression.CompileFast overloads for Delegate, Funcs, and Actions

        /// <summary>Compiles lambda expression to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static TDelegate CompileFast<TDelegate>(this LambdaExpression lambdaExpr, bool ifFastFailedReturnNull = false)
            where TDelegate : class =>
            TryCompile<TDelegate>(lambdaExpr) ??
            (ifFastFailedReturnNull ? null : (TDelegate)(object)lambdaExpr.Compile());

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Delegate CompileFast(this LambdaExpression lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Delegate>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<R> CompileFast<R>(this Expression<Func<R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, R> CompileFast<T1, R>(this Expression<Func<T1, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, R> CompileFast<T1, T2, R>(this Expression<Func<T1, T2, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, R> CompileFast<T1, T2, T3, R>(
            this Expression<Func<T1, T2, T3, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, R> CompileFast<T1, T2, T3, T4, R>(
            this Expression<Func<T1, T2, T3, T4, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, T5, R> CompileFast<T1, T2, T3, T4, T5, R>(
            this Expression<Func<T1, T2, T3, T4, T5, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, T5, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, T5, T6, R> CompileFast<T1, T2, T3, T4, T5, T6, R>(
            this Expression<Func<T1, T2, T3, T4, T5, T6, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, T5, T6, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action CompileFast(this Expression<Action> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1> CompileFast<T1>(this Expression<Action<T1>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2> CompileFast<T1, T2>(this Expression<Action<T1, T2>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3> CompileFast<T1, T2, T3>(this Expression<Action<T1, T2, T3>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4> CompileFast<T1, T2, T3, T4>(
            this Expression<Action<T1, T2, T3, T4>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4, T5> CompileFast<T1, T2, T3, T4, T5>(
            this Expression<Action<T1, T2, T3, T4, T5>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4, T5>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4, T5, T6> CompileFast<T1, T2, T3, T4, T5, T6>(
            this Expression<Action<T1, T2, T3, T4, T5, T6>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4, T5, T6>>(ifFastFailedReturnNull);

        #endregion

        #region ExpressionInfo.CompileFast overloads for Delegate, Funcs, and Actions

        /// <summary>Compiles lambda expression info to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static TDelegate CompileFast<TDelegate>(this LambdaExpressionInfo lambdaExpr, bool ifFastFailedReturnNull = false)
            where TDelegate : class =>
            TryCompile<TDelegate>(lambdaExpr) ??
            (ifFastFailedReturnNull ? null : (TDelegate)(object)lambdaExpr.ToLambdaExpression().Compile());

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Delegate CompileFast(this LambdaExpressionInfo lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Delegate>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<R> CompileFast<R>(this ExpressionInfo<Func<R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, R> CompileFast<T1, R>(this ExpressionInfo<Func<T1, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, R> CompileFast<T1, T2, R>(this ExpressionInfo<Func<T1, T2, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, R> CompileFast<T1, T2, T3, R>(
            this ExpressionInfo<Func<T1, T2, T3, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to TDelegate type. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, R> CompileFastInfo<T1, T2, T3, T4, R>(
            this Expression<Func<T1, T2, T3, T4, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, T5, R> CompileFast<T1, T2, T3, T4, T5, R>(
            this ExpressionInfo<Func<T1, T2, T3, T4, T5, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, T5, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Func<T1, T2, T3, T4, T5, T6, R> CompileFast<T1, T2, T3, T4, T5, T6, R>(
            this ExpressionInfo<Func<T1, T2, T3, T4, T5, T6, R>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Func<T1, T2, T3, T4, T5, T6, R>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action CompileFast(this ExpressionInfo<Action> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1> CompileFast<T1>(this ExpressionInfo<Action<T1>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2> CompileFast<T1, T2>(this ExpressionInfo<Action<T1, T2>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3> CompileFast<T1, T2, T3>(this ExpressionInfo<Action<T1, T2, T3>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4> CompileFast<T1, T2, T3, T4>(
            this ExpressionInfo<Action<T1, T2, T3, T4>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4, T5> CompileFast<T1, T2, T3, T4, T5>(
            this ExpressionInfo<Action<T1, T2, T3, T4, T5>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4, T5>>(ifFastFailedReturnNull);

        /// <summary>Compiles lambda expression info to delegate. Use ifFastFailedReturnNull parameter to Not fallback to Expression.Compile, useful for testing.</summary>
        public static Action<T1, T2, T3, T4, T5, T6> CompileFast<T1, T2, T3, T4, T5, T6>(
            this ExpressionInfo<Action<T1, T2, T3, T4, T5, T6>> lambdaExpr, bool ifFastFailedReturnNull = false) =>
            lambdaExpr.CompileFast<Action<T1, T2, T3, T4, T5, T6>>(ifFastFailedReturnNull);

        #endregion

        /// <summary>Tries to compile lambda expression to <typeparamref name="TDelegate"/>.</summary>
        public static TDelegate TryCompile<TDelegate>(this LambdaExpression lambdaExpr)
            where TDelegate : class =>
            TryCompile<TDelegate>(lambdaExpr.Body, lambdaExpr.Parameters,
                Tools.GetParamExprTypes(lambdaExpr.Parameters), lambdaExpr.Body.Type);

        /// <summary>Compiles expression to delegate by emitting the IL. 
        /// If sub-expressions are not supported by emitter, then the method returns null.
        /// The usage should be calling the method, if result is null then calling the Expression.Compile.</summary>
        /// <param name="bodyExpr">Lambda body.</param>
        /// <param name="paramExprs">Lambda parameter expressions.</param>
        /// <param name="paramTypes">The types of parameters.</param>
        /// <param name="returnType">The return type.</param>
        /// <returns>Result delegate or null, if unable to compile.</returns>
        public static TDelegate TryCompile<TDelegate>(
            Expression bodyExpr, IList<ParameterExpression> paramExprs, Type[] paramTypes,
            Type returnType) where TDelegate : class
        {
            var paramArray = paramExprs as ParameterExpression[] ?? paramExprs.ToArray(); // todo: Improve, better remove
            ClosureInfo ignored = null;
            return (TDelegate)TryCompile(ref ignored, typeof(TDelegate),
                paramTypes, returnType, bodyExpr, bodyExpr.NodeType, bodyExpr.Type, paramArray);
        }

        /// <summary>Tries to compile lambda expression info.</summary>
        /// <typeparam name="TDelegate">The compatible delegate type, otherwise case will throw.</typeparam>
        /// <param name="lambdaExpr">Lambda expression to compile.</param>
        /// <returns>Compiled delegate or null.</returns>
        public static TDelegate TryCompile<TDelegate>(this LambdaExpressionInfo lambdaExpr)
            where TDelegate : class =>
            TryCompile<TDelegate>(lambdaExpr.Body, lambdaExpr.Parameters,
                Tools.GetParamExprTypes(lambdaExpr.Parameters), lambdaExpr.Body.GetResultType());

        /// <summary>Tries to compile lambda expression info.</summary>
        public static Delegate TryCompile(this LambdaExpressionInfo lambdaExpr) =>
            TryCompile<Delegate>(lambdaExpr);

        /// <summary>Tries to compile lambda expression info.</summary>
        public static TDelegate TryCompile<TDelegate>(this ExpressionInfo<TDelegate> lambdaExpr)
            where TDelegate : class =>
            TryCompile<TDelegate>((LambdaExpressionInfo)lambdaExpr);

        /// <summary>Compiles expression to delegate by emitting the IL. 
        /// If sub-expressions are not supported by emitter, then the method returns null.
        /// The usage should be calling the method, if result is null then calling the Expression.Compile.</summary>
        public static TDelegate TryCompile<TDelegate>(
            ExpressionInfo bodyExpr, IList<ParameterExpression> paramExprs, Type[] paramTypes,
            Type returnType) where TDelegate : class
        {
            var paramArray = paramExprs as ParameterExpression[] ?? paramExprs.ToArray();
            ClosureInfo ignored = null;
            return (TDelegate)TryCompile(ref ignored, typeof(TDelegate),
                paramTypes, returnType, bodyExpr, bodyExpr.NodeType, returnType, paramArray);
        }

        /// <summary>Compiles expression to delegate by emitting the IL. 
        /// If sub-expressions are not supported by emitter, then the method returns null.
        /// The usage should be calling the method, if result is null then calling the Expression.Compile.</summary>
        public static TDelegate TryCompile<TDelegate>(
            ExpressionInfo bodyExpr, IList<ParameterExpressionInfo> paramExprs, Type[] paramTypes,
            Type returnType) where TDelegate : class
        {
            var paramArray = paramExprs as ParameterExpressionInfo[] ?? paramExprs.ToArray();
            ClosureInfo ignored = null;
            return (TDelegate)TryCompile(ref ignored, typeof(TDelegate),
                paramTypes, returnType, bodyExpr, bodyExpr.NodeType, returnType, paramArray);
        }

        /// <summary>Compiles expression to delegate by emitting the IL. 
        /// If sub-expressions are not supported by emitter, then the method returns null.
        /// The usage should be calling the method, if result is null then calling the Expression.Compile.</summary>
        public static TDelegate TryCompile<TDelegate>(
            object bodyExpr, object[] paramExprs, Type[] paramTypes,
            Type returnType) where TDelegate : class
        {
            ClosureInfo ignored = null;
            return (TDelegate)TryCompile(ref ignored, typeof(TDelegate),
                paramTypes, returnType, bodyExpr, bodyExpr.GetNodeType(), returnType, paramExprs);
        }

        private static object TryCompile(ref ClosureInfo closureInfo,
            Type delegateType, Type[] paramTypes, Type returnType,
            object exprObj, ExpressionType exprNodeType, Type exprType, object[] paramExprs,
            bool isNestedLambda = false)
        {
            if (!TryCollectBoundConstants(ref closureInfo, exprObj, exprNodeType, exprType, paramExprs))
                return null;

            closureInfo?.FinishAnalysis();

            if (closureInfo == null || !closureInfo.HasBoundClosure)
                return TryCompileStaticDelegate(delegateType,
                    paramTypes, returnType, exprObj, exprNodeType, exprType, paramExprs);

            var closureObject = closureInfo.ConstructClosure(closureTypeOnly: isNestedLambda);
            var closureAndParamTypes = GetClosureAndParamTypes(paramTypes, closureInfo.ClosureType);

            var methodWithClosure = new DynamicMethod(string.Empty, returnType, closureAndParamTypes,
                closureInfo.ClosureType, skipVisibility: true);

            if (!TryEmit(methodWithClosure, exprObj, exprNodeType, exprType, paramExprs, closureInfo))
                return null;

            // include closure as the first parameter, BUT don't bound to it. It will be bound later in EmitNestedLambda.
            if (isNestedLambda)
                return methodWithClosure.CreateDelegate(Tools.GetFuncOrActionType(closureAndParamTypes, returnType));

            // create a specific delegate if user requested delegate is untyped, otherwise CreateMethod will fail
            if (delegateType == typeof(Delegate))
                delegateType = Tools.GetFuncOrActionType(paramTypes, returnType);

            return methodWithClosure.CreateDelegate(delegateType, closureObject);
        }

        private static object TryCompileStaticDelegate(Type delegateType, Type[] paramTypes, Type returnType, object exprObj,
            ExpressionType exprNodeType, Type exprType, object[] paramExprs)
        {
            var method = new DynamicMethod(string.Empty, returnType, paramTypes,
                typeof(ExpressionCompiler), skipVisibility: true);

            if (!TryEmit(method, exprObj, exprNodeType, exprType, paramExprs, null))
                return null;

            // create a specific delegate if user requested delegate is untyped, otherwise CreateMethod will fail
            if (delegateType == typeof(Delegate))
                delegateType = Tools.GetFuncOrActionType(paramTypes, returnType);

            return method.CreateDelegate(delegateType);
        }

        private static bool TryEmit(DynamicMethod method,
            object exprObj, ExpressionType exprNodeType, Type exprType, object[] paramExprs, ClosureInfo closureInfo)
        {
            var il = method.GetILGenerator();
            if (!EmittingVisitor.TryEmit(exprObj, exprNodeType, exprType, paramExprs, il, closureInfo))
                return false;

            il.Emit(OpCodes.Ret); // emits return from generated method
            return true;
        }

        private static Type[] GetClosureAndParamTypes(Type[] paramTypes, Type closureType)
        {
            var paramCount = paramTypes.Length;
            if (paramCount == 0)
                return new[] { closureType };

            if (paramCount == 1)
                return new[] { closureType, paramTypes[0] };

            var closureAndParamTypes = new Type[paramCount + 1];
            closureAndParamTypes[0] = closureType;
            Array.Copy(paramTypes, 0, closureAndParamTypes, 1, paramCount);
            return closureAndParamTypes;
        }

        private sealed class BlockInfo
        {
            public static readonly BlockInfo Empty = new BlockInfo();

            public bool IsEmpty => Parent == null;
            public readonly BlockInfo Parent;
            public readonly object ResultExpr;
            public readonly object[] VarExprs;
            public readonly LocalBuilder[] LocalVars;

            private BlockInfo() { }

            internal BlockInfo(BlockInfo parent,
                object resultExpr, object[] varExprs, LocalBuilder[] localVars)
            {
                Parent = parent;
                ResultExpr = resultExpr;
                VarExprs = varExprs;
                LocalVars = localVars;
            }
        }

        [DebuggerDisplay("Expression={ConstantExpr}")]
        private struct ConstantInfo
        {
            public object ConstantExpr, Value;
            public Type Type;
            public ConstantInfo(object constantExpr, object value, Type type)
            {
                ConstantExpr = constantExpr;
                Value = value;
                Type = type;
            }
        }

        // todo: Rename to Context in next major version, cause it is not only about closure anymore
        // Track the info required to build a closure object + some context information not directly related to closure.
        private sealed class ClosureInfo
        {
            // Closed values used by expression and by its nested lambdas
            public ConstantInfo[] Constants = Tools.Empty<ConstantInfo>();

            // Parameters not passed through lambda parameter list But used inside lambda body.
            // The top expression should not! contain non passed parameters. 
            public object[] NonPassedParameters = Tools.Empty<object>();

            // All nested lambdas recursively nested in expression
            public NestedLambdaInfo[] NestedLambdas = Tools.Empty<NestedLambdaInfo>();

            // FieldInfos are needed to load field of closure object on stack in emitter
            // It is also an indicator that we use typed Closure object and not an array
            public FieldInfo[] Fields { get; private set; }

            // Type of constructed closure, is known after ConstructClosure call
            public Type ClosureType { get; private set; }

            // Known after ConstructClosure call
            public int ClosedItemCount { get; private set; }

            // Helper member to decide when we are inside in a block or not
            public BlockInfo CurrentBlock = BlockInfo.Empty;

            // Tells that we should construct a bounded closure object for the compiled delegate,
            // also indicates that we have to shift when we are operating on arguments 
            // because the first argument should be the closure
            public bool HasBoundClosure { get; private set; }

            public void AddConstant(object expr, object value, Type type)
            {
                if (Constants.Length == 0 ||
                    Constants.GetFirstIndex(it => it.ConstantExpr == expr) == -1)
                    Constants = Constants.WithLast(new ConstantInfo(expr, value, type));
            }

            public void AddConstant(ConstantInfo info)
            {
                if (Constants.Length == 0 ||
                    Constants.GetFirstIndex(it => it.ConstantExpr == info.ConstantExpr) == -1)
                    Constants = Constants.WithLast(info);
            }

            public void AddNonPassedParam(object exprObj)
            {
                if (NonPassedParameters.Length == 0 ||
                    NonPassedParameters.GetFirstIndex(exprObj) == -1)
                    NonPassedParameters = NonPassedParameters.WithLast(exprObj);
            }

            public void AddNestedLambda(object lambdaExpr, object lambda, ClosureInfo closureInfo, bool isAction)
            {
                if (NestedLambdas.Length == 0 ||
                    NestedLambdas.GetFirstIndex(it => it.LambdaExpr == lambdaExpr) == -1)
                    NestedLambdas = NestedLambdas.WithLast(new NestedLambdaInfo(closureInfo, lambdaExpr, lambda, isAction));
            }

            public void AddNestedLambda(NestedLambdaInfo info)
            {
                if (NestedLambdas.Length == 0 ||
                    NestedLambdas.GetFirstIndex(it => it.LambdaExpr == info.LambdaExpr) == -1)
                    NestedLambdas = NestedLambdas.WithLast(info);
            }

            public object ConstructClosure(bool closureTypeOnly)
            {
                var constants = Constants;
                var nonPassedParams = NonPassedParameters;
                var nestedLambdas = NestedLambdas;

                var constPlusParamCount = constants.Length + nonPassedParams.Length;
                var totalItemCount = constPlusParamCount + nestedLambdas.Length;

                ClosedItemCount = totalItemCount;

                var closureCreateMethods = Closure.CreateMethods;

                // Construct the array based closure when number of values is bigger than
                // number of fields in biggest supported Closure class.
                if (totalItemCount > closureCreateMethods.Length)
                {
                    ClosureType = typeof(ArrayClosure);

                    if (closureTypeOnly)
                        return null;

                    var items = new object[totalItemCount];
                    if (constants.Length != 0)
                        for (var i = 0; i < constants.Length; i++)
                            items[i] = constants[i].Value;

                    // skip non passed parameters as it is only for nested lambdas

                    if (nestedLambdas.Length != 0)
                        for (var i = 0; i < nestedLambdas.Length; i++)
                            items[constPlusParamCount + i] = nestedLambdas[i].Lambda;

                    return new ArrayClosure(items);
                }

                // Construct the Closure Type and optionally Closure object with closed values stored as fields:
                object[] fieldValues = null;
                var fieldTypes = new Type[totalItemCount];
                if (closureTypeOnly)
                {
                    if (constants.Length != 0)
                        for (var i = 0; i < constants.Length; i++)
                            fieldTypes[i] = constants[i].Type;

                    if (nonPassedParams.Length != 0)
                        for (var i = 0; i < nonPassedParams.Length; i++)
                            fieldTypes[constants.Length + i] = nonPassedParams[i].GetResultType();

                    if (nestedLambdas.Length != 0)
                        for (var i = 0; i < nestedLambdas.Length; i++)
                            fieldTypes[constPlusParamCount + i] = nestedLambdas[i].Lambda.GetType();
                }
                else
                {
                    fieldValues = new object[totalItemCount];

                    if (constants.Length != 0)
                        for (var i = 0; i < constants.Length; i++)
                        {
                            var constantExpr = constants[i];
                            fieldTypes[i] = constantExpr.Type;
                            fieldValues[i] = constantExpr.Value;
                        }

                    if (nonPassedParams.Length != 0)
                        for (var i = 0; i < nonPassedParams.Length; i++)
                            fieldTypes[constants.Length + i] = nonPassedParams[i].GetResultType();

                    if (nestedLambdas.Length != 0)
                        for (var i = 0; i < nestedLambdas.Length; i++)
                        {
                            var lambda = nestedLambdas[i].Lambda;
                            fieldValues[constPlusParamCount + i] = lambda;
                            fieldTypes[constPlusParamCount + i] = lambda.GetType();
                        }
                }

                var createClosureMethod = closureCreateMethods[totalItemCount - 1];
                var createClosure = createClosureMethod.MakeGenericMethod(fieldTypes);
                ClosureType = createClosure.ReturnType;

                var fields = ClosureType.GetTypeInfo().DeclaredFields;
                Fields = fields as FieldInfo[] ?? fields.ToArray();

                if (fieldValues == null)
                    return null;
                return createClosure.Invoke(null, fieldValues);
            }

            public void FinishAnalysis() =>
                HasBoundClosure = Constants.Length != 0 || NestedLambdas.Length != 0 || NonPassedParameters.Length != 0;

            public void PushBlock(object blockResultExpr, object[] blockVarExprs, LocalBuilder[] localVars) =>
                CurrentBlock = new BlockInfo(CurrentBlock, blockResultExpr, blockVarExprs, localVars);

            public void PushBlockAndConstructLocalVars(object blockResultExpr, object[] blockVarExprs, ILGenerator il)
            {
                var localVars = Tools.Empty<LocalBuilder>();
                if (blockVarExprs.Length != 0)
                {
                    localVars = new LocalBuilder[blockVarExprs.Length];
                    for (var i = 0; i < localVars.Length; i++)
                        localVars[i] = il.DeclareLocal(blockVarExprs[i].GetResultType());
                }

                CurrentBlock = new BlockInfo(CurrentBlock, blockResultExpr, blockVarExprs, localVars);
            }

            public void PopBlock() =>
                CurrentBlock = CurrentBlock.Parent;

            public bool IsLocalVar(object varParamExpr)
            {
                var i = -1;
                for (var block = CurrentBlock; i == -1 && !block.IsEmpty; block = block.Parent)
                    i = block.VarExprs.GetFirstIndex(varParamExpr);
                return i != -1;
            }

            public LocalBuilder GetDefinedLocalVarOrDefault(object varParamExpr)
            {
                for (var block = CurrentBlock; !block.IsEmpty; block = block.Parent)
                {
                    if (block.LocalVars.Length == 0)
                        continue;
                    var varIndex = block.VarExprs.GetFirstIndex(varParamExpr);
                    if (varIndex != -1)
                        return block.LocalVars[varIndex];
                }
                return null;
            }
        }

        #region Closures

        internal static class Closure
        {
            private static readonly IEnumerable<MethodInfo> _methods =
                typeof(Closure).GetTypeInfo().DeclaredMethods;

            public static readonly MethodInfo[] CreateMethods =
                _methods as MethodInfo[] ?? _methods.ToArray();

            public static Closure<T1> CreateClosure<T1>(T1 v1) => new Closure<T1>(v1);

            public static Closure<T1, T2> CreateClosure<T1, T2>(T1 v1, T2 v2) => new Closure<T1, T2>(v1, v2);

            public static Closure<T1, T2, T3> CreateClosure<T1, T2, T3>(T1 v1, T2 v2, T3 v3) =>
                new Closure<T1, T2, T3>(v1, v2, v3);

            public static Closure<T1, T2, T3, T4> CreateClosure<T1, T2, T3, T4>(T1 v1, T2 v2, T3 v3, T4 v4) =>
                new Closure<T1, T2, T3, T4>(v1, v2, v3, v4);

            public static Closure<T1, T2, T3, T4, T5> CreateClosure<T1, T2, T3, T4, T5>(T1 v1, T2 v2, T3 v3, T4 v4,
                T5 v5) => new Closure<T1, T2, T3, T4, T5>(v1, v2, v3, v4, v5);

            public static Closure<T1, T2, T3, T4, T5, T6> CreateClosure<T1, T2, T3, T4, T5, T6>(T1 v1, T2 v2, T3 v3,
                T4 v4, T5 v5, T6 v6) => new Closure<T1, T2, T3, T4, T5, T6>(v1, v2, v3, v4, v5, v6);

            public static Closure<T1, T2, T3, T4, T5, T6, T7> CreateClosure<T1, T2, T3, T4, T5, T6, T7>(T1 v1, T2 v2,
                T3 v3, T4 v4, T5 v5, T6 v6, T7 v7) =>
                new Closure<T1, T2, T3, T4, T5, T6, T7>(v1, v2, v3, v4, v5, v6, v7);

            public static Closure<T1, T2, T3, T4, T5, T6, T7, T8> CreateClosure<T1, T2, T3, T4, T5, T6, T7, T8>(
                T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8) =>
                new Closure<T1, T2, T3, T4, T5, T6, T7, T8>(v1, v2, v3, v4, v5, v6, v7, v8);

            public static Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9> CreateClosure<T1, T2, T3, T4, T5, T6, T7, T8, T9>(
                T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8, T9 v9) =>
                new Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9>(v1, v2, v3, v4, v5, v6, v7, v8, v9);

            public static Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> CreateClosure<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>(
                T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8, T9 v9, T10 v10) =>
                new Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10);
        }

        internal sealed class Closure<T1>
        {
            public T1 V1;

            public Closure(T1 v1)
            {
                V1 = v1;
            }
        }

        internal sealed class Closure<T1, T2>
        {
            public T1 V1;
            public T2 V2;

            public Closure(T1 v1, T2 v2)
            {
                V1 = v1;
                V2 = v2;
            }
        }

        internal sealed class Closure<T1, T2, T3>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;

            public Closure(T1 v1, T2 v2, T3 v3)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5, T6>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;
            public T6 V6;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
                V6 = v6;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5, T6, T7>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;
            public T6 V6;
            public T7 V7;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
                V6 = v6;
                V7 = v7;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5, T6, T7, T8>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;
            public T6 V6;
            public T7 V7;
            public T8 V8;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
                V6 = v6;
                V7 = v7;
                V8 = v8;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;
            public T6 V6;
            public T7 V7;
            public T8 V8;
            public T9 V9;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8, T9 v9)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
                V6 = v6;
                V7 = v7;
                V8 = v8;
                V9 = v9;
            }
        }

        internal sealed class Closure<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>
        {
            public T1 V1;
            public T2 V2;
            public T3 V3;
            public T4 V4;
            public T5 V5;
            public T6 V6;
            public T7 V7;
            public T8 V8;
            public T9 V9;
            public T10 V10;

            public Closure(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8, T9 v9, T10 v10)
            {
                V1 = v1;
                V2 = v2;
                V3 = v3;
                V4 = v4;
                V5 = v5;
                V6 = v6;
                V7 = v7;
                V8 = v8;
                V9 = v9;
                V10 = v10;
            }
        }

        internal sealed class ArrayClosure
        {
            public readonly object[] Constants;

            public static FieldInfo ArrayField = typeof(ArrayClosure).GetTypeInfo().DeclaredFields.GetFirst(f => !f.IsStatic);
            public static ConstructorInfo Constructor = typeof(ArrayClosure).GetTypeInfo().DeclaredConstructors.GetFirst();

            public ArrayClosure(object[] constants)
            {
                Constants = constants;
            }
        }

        #endregion

        #region Nested Lambdas

        private struct NestedLambdaInfo
        {
            public ClosureInfo ClosureInfo;

            public object LambdaExpr; // to find the lambda in bigger parent expression
            public object Lambda;
            public bool IsAction;

            public NestedLambdaInfo(ClosureInfo closureInfo, object lambdaExpr, object lambda, bool isAction)
            {
                ClosureInfo = closureInfo;
                Lambda = lambda;
                LambdaExpr = lambdaExpr;
                IsAction = isAction;
            }
        }

        internal static class CurryClosureFuncs
        {
            private static readonly IEnumerable<MethodInfo> _methods =
                typeof(CurryClosureFuncs).GetTypeInfo().DeclaredMethods;

            public static readonly MethodInfo[] Methods = _methods as MethodInfo[] ?? _methods.ToArray();

            public static Func<R> Curry<C, R>(Func<C, R> f, C c) { return () => f(c); }
            public static Func<T1, R> Curry<C, T1, R>(Func<C, T1, R> f, C c) { return t1 => f(c, t1); }
            public static Func<T1, T2, R> Curry<C, T1, T2, R>(Func<C, T1, T2, R> f, C c) { return (t1, t2) => f(c, t1, t2); }
            public static Func<T1, T2, T3, R> Curry<C, T1, T2, T3, R>(Func<C, T1, T2, T3, R> f, C c) { return (t1, t2, t3) => f(c, t1, t2, t3); }
            public static Func<T1, T2, T3, T4, R> Curry<C, T1, T2, T3, T4, R>(Func<C, T1, T2, T3, T4, R> f, C c) { return (t1, t2, t3, t4) => f(c, t1, t2, t3, t4); }
            public static Func<T1, T2, T3, T4, T5, R> Curry<C, T1, T2, T3, T4, T5, R>(Func<C, T1, T2, T3, T4, T5, R> f, C c) { return (t1, t2, t3, t4, t5) => f(c, t1, t2, t3, t4, t5); }
            public static Func<T1, T2, T3, T4, T5, T6, R> Curry<C, T1, T2, T3, T4, T5, T6, R>(Func<C, T1, T2, T3, T4, T5, T6, R> f, C c) { return (t1, t2, t3, t4, t5, t6) => f(c, t1, t2, t3, t4, t5, t6); }
        }

        internal static class CurryClosureActions
        {
            private static readonly IEnumerable<MethodInfo> _methods =
                typeof(CurryClosureActions).GetTypeInfo().DeclaredMethods;

            public static readonly MethodInfo[] Methods = _methods as MethodInfo[] ?? _methods.ToArray();

            internal static Action Curry<C>(Action<C> a, C c) { return () => a(c); }
            internal static Action<T1> Curry<C, T1>(Action<C, T1> f, C c) { return t1 => f(c, t1); }
            internal static Action<T1, T2> Curry<C, T1, T2>(Action<C, T1, T2> f, C c) { return (t1, t2) => f(c, t1, t2); }
            internal static Action<T1, T2, T3> Curry<C, T1, T2, T3>(Action<C, T1, T2, T3> f, C c) { return (t1, t2, t3) => f(c, t1, t2, t3); }
            internal static Action<T1, T2, T3, T4> Curry<C, T1, T2, T3, T4>(Action<C, T1, T2, T3, T4> f, C c) { return (t1, t2, t3, t4) => f(c, t1, t2, t3, t4); }
            internal static Action<T1, T2, T3, T4, T5> Curry<C, T1, T2, T3, T4, T5>(Action<C, T1, T2, T3, T4, T5> f, C c) { return (t1, t2, t3, t4, t5) => f(c, t1, t2, t3, t4, t5); }
            internal static Action<T1, T2, T3, T4, T5, T6> Curry<C, T1, T2, T3, T4, T5, T6>(Action<C, T1, T2, T3, T4, T5, T6> f, C c) { return (t1, t2, t3, t4, t5, t6) => f(c, t1, t2, t3, t4, t5, t6); }
        }

        #endregion

        #region Collect Bound Constants

        private static bool IsBoundConstant(object value)
        {
            if (value == null)
                return false;

            var typeInfo = value.GetType().GetTypeInfo();
            return !typeInfo.IsPrimitive
                   && !(value is string)
                   && !(value is Type)
                   && !typeInfo.IsEnum;
        }

        // @paramExprs is required for nested lambda compilation
        private static bool TryCollectBoundConstants(ref ClosureInfo closure,
            object exprObj, ExpressionType exprNodeType, Type exprType, object[] paramExprs)
        {
            if (exprObj == null)
                return false;

            switch (exprNodeType)
            {
                case ExpressionType.Constant:
                    var constExprInfo = exprObj as ConstantExpressionInfo;
                    var value = constExprInfo != null ? constExprInfo.Value : ((ConstantExpression)exprObj).Value;
                    if (value is Delegate || IsBoundConstant(value))
                        (closure ?? (closure = new ClosureInfo())).AddConstant(exprObj, value, exprType);
                    return true;

                case ExpressionType.Parameter:
                    // if parameter is used BUT is not in passed parameters and not in local variables,
                    // it means parameter is provided by outer lambda and should be put in closure for current lambda
                    if (paramExprs.GetFirstIndex(exprObj) == -1 &&
                        (closure == null || !closure.IsLocalVar(exprObj)))
                        (closure ?? (closure = new ClosureInfo())).AddNonPassedParam(exprObj);
                    return true;

                case ExpressionType.Call:
                    return TryCollectCallExprConstants(ref closure, exprObj, paramExprs);

                case ExpressionType.MemberAccess:
                    var memberExprInfo = exprObj as MemberExpressionInfo;
                    if (memberExprInfo != null)
                    {
                        var maExpr = memberExprInfo.Expression;
                        return maExpr == null
                            || TryCollectBoundConstants(ref closure, maExpr, maExpr.GetNodeType(), maExpr.GetResultType(), paramExprs);
                    }

                    var memberExpr = ((MemberExpression)exprObj).Expression;
                    return memberExpr == null
                        || TryCollectBoundConstants(ref closure, memberExpr, memberExpr.NodeType, memberExpr.Type, paramExprs);

                case ExpressionType.New:
                    var newExprInfo = exprObj as NewExpressionInfo;
                    return newExprInfo != null
                        ? TryCollectBoundConstants(ref closure, newExprInfo.Arguments, paramExprs)
                        : TryCollectBoundConstants(ref closure, ((NewExpression)(Expression)exprObj).Arguments, paramExprs);

                case ExpressionType.NewArrayBounds:
                case ExpressionType.NewArrayInit:
                    var newArrayInitInfo = exprObj as NewArrayExpressionInfo;
                    return newArrayInitInfo != null ?
                        TryCollectBoundConstants(ref closure, newArrayInitInfo.Arguments, paramExprs) :
                        TryCollectBoundConstants(ref closure, ((NewArrayExpression)exprObj).Expressions, paramExprs);

                case ExpressionType.MemberInit:
                    return TryCollectMemberInitExprConstants(ref closure, exprObj, paramExprs);

                case ExpressionType.Lambda:
                    return TryCompileNestedLambda(ref closure, exprObj, paramExprs);

                case ExpressionType.Invoke:
                    var invokeExpr = exprObj as InvocationExpression;
                    if (invokeExpr != null)
                    {
                        var lambda = invokeExpr.Expression;
                        return TryCollectBoundConstants(ref closure, lambda, lambda.NodeType, lambda.Type, paramExprs)
                            && TryCollectBoundConstants(ref closure, invokeExpr.Arguments, paramExprs);
                    }
                    else
                    {
                        var invokeInfo = (InvocationExpressionInfo)exprObj;
                        var lambda = invokeInfo.ExprToInvoke;
                        return TryCollectBoundConstants(ref closure, lambda, lambda.NodeType, lambda.Type, paramExprs)
                            && TryCollectBoundConstants(ref closure, invokeInfo.Arguments, paramExprs);
                    }

                case ExpressionType.Conditional:
                    var condExpr = (ConditionalExpression)exprObj;
                    return TryCollectBoundConstants(ref closure, condExpr.Test, condExpr.Test.NodeType, condExpr.Type, paramExprs)
                        && TryCollectBoundConstants(ref closure, condExpr.IfTrue, condExpr.IfTrue.NodeType, condExpr.Type, paramExprs)
                        && TryCollectBoundConstants(ref closure, condExpr.IfFalse, condExpr.IfFalse.NodeType, condExpr.IfFalse.Type, paramExprs);

                case ExpressionType.Block:
                    return TryCollectBlockBoundConstants(ref closure, exprObj, paramExprs);

                case ExpressionType.Index:
                    var indexExpr = (IndexExpression)exprObj;
                    var obj = indexExpr.Object;
                    return obj == null
                        || TryCollectBoundConstants(ref closure, indexExpr.Object, indexExpr.Object.NodeType, indexExpr.Object.Type, paramExprs)
                        && TryCollectBoundConstants(ref closure, indexExpr.Arguments, paramExprs);

                case ExpressionType.Try:
                    return exprObj is TryExpression
                        ? TryCollectTryExprConstants(ref closure, (TryExpression)exprObj, paramExprs)
                        : TryCollectTryExprInfoConstants(ref closure, (TryExpressionInfo)exprObj, paramExprs);

                case ExpressionType.Default:
                    return true;

                default:
                    return TryCollectUnaryOrBinaryExprConstants(ref closure, exprObj, paramExprs);
            }
        }

        private static bool TryCollectBlockBoundConstants(ref ClosureInfo closure, object exprObj, object[] paramExprs)
        {
            closure = closure ?? new ClosureInfo();
            var blockExpr = exprObj as BlockExpression;
            if (blockExpr != null)
            {
                closure.PushBlock(blockExpr.Result, blockExpr.Variables.ToArray(), Tools.Empty<LocalBuilder>());
                if (!TryCollectBoundConstants(ref closure, blockExpr.Expressions, paramExprs))
                    return false;
            }
            else
            {
                var blockExprInfo = (BlockExpressionInfo)exprObj;
                closure.PushBlock(blockExprInfo.Result, blockExprInfo.Variables, Tools.Empty<LocalBuilder>());
                if (!TryCollectBoundConstants(ref closure, blockExprInfo.Expressions, paramExprs))
                    return false;
            }

            closure.PopBlock();
            return true;
        }

        private static bool TryCollectBoundConstants(ref ClosureInfo closure,
            object[] exprObjects, object[] paramExprs)
        {
            for (var i = 0; i < exprObjects.Length; i++)
            {
                var exprObj = exprObjects[i];
                var exprMeta = GetExpressionMeta(exprObj);
                if (!TryCollectBoundConstants(ref closure, exprObj, exprMeta.Key, exprMeta.Value, paramExprs))
                    return false;
            }
            return true;
        }

        private static bool TryCompileNestedLambda(
            ref ClosureInfo closure, object exprObj, object[] paramExprs)
        {
            // 1. Try to compile nested lambda in place
            // 2. Check that parameters used in compiled lambda are passed or closed by outer lambda
            // 3. Add the compiled lambda to closure of outer lambda for later invocation

            object compiledLambda;
            Type bodyType;
            ClosureInfo nestedClosure = null;

            var lambdaExprInfo = exprObj as LambdaExpressionInfo;
            if (lambdaExprInfo != null)
            {
                var lambdaParamExprs = lambdaExprInfo.Parameters;
                var body = lambdaExprInfo.Body;
                bodyType = body.GetResultType();
                compiledLambda = TryCompile(ref nestedClosure,
                    lambdaExprInfo.Type, Tools.GetParamExprTypes(lambdaParamExprs), bodyType,
                    body, body.GetNodeType(), bodyType,
                    lambdaParamExprs, isNestedLambda: true);
            }
            else
            {
                var lambdaExpr = (LambdaExpression)exprObj;
                object[] lambdaParamExprs = lambdaExpr.Parameters.ToArray();
                var bodyExpr = lambdaExpr.Body;
                bodyType = bodyExpr.Type;
                compiledLambda = TryCompile(ref nestedClosure,
                    lambdaExpr.Type, Tools.GetParamExprTypes(lambdaParamExprs), bodyType,
                    bodyExpr, bodyExpr.NodeType, bodyExpr.Type,
                    lambdaParamExprs, isNestedLambda: true);
            }

            if (compiledLambda == null)
                return false;

            // add the nested lambda into closure
            (closure ?? (closure = new ClosureInfo()))
                .AddNestedLambda(exprObj, compiledLambda, nestedClosure, isAction: bodyType == typeof(void));

            if (nestedClosure == null)
                return true; // done

            // if nested non passed parameter is no matched with any outer passed parameter, 
            // then ensure it goes to outer non passed parameter.
            // But check that have non passed parameter in root expression is invalid.
            var nestedNonPassedParams = nestedClosure.NonPassedParameters;
            if (nestedNonPassedParams.Length != 0)
                for (var i = 0; i < nestedNonPassedParams.Length; i++)
                {
                    var nestedNonPassedParam = nestedNonPassedParams[i];
                    if (paramExprs.Length == 0 ||
                        paramExprs.GetFirstIndex(nestedNonPassedParam) == -1)
                        closure.AddNonPassedParam(nestedNonPassedParam);
                }

            // Promote found constants and nested lambdas into outer closure
            var nestedConstants = nestedClosure.Constants;
            if (nestedConstants.Length != 0)
                for (var i = 0; i < nestedConstants.Length; i++)
                    closure.AddConstant(nestedConstants[i]);

            var nestedNestedLambdas = nestedClosure.NestedLambdas;
            if (nestedNestedLambdas.Length != 0)
                for (var i = 0; i < nestedNestedLambdas.Length; i++)
                    closure.AddNestedLambda(nestedNestedLambdas[i]);

            return true;

        }

        private static bool TryCollectMemberInitExprConstants(ref ClosureInfo closure, object exprObj, object[] paramExprs)
        {
            var memberInitExprInfo = exprObj as MemberInitExpressionInfo;
            if (memberInitExprInfo != null)
            {
                var miNewInfo = memberInitExprInfo.ExpressionInfo;
                if (!TryCollectBoundConstants(ref closure, miNewInfo, miNewInfo.NodeType, miNewInfo.Type, paramExprs))
                    return false;

                var memberBindingInfos = memberInitExprInfo.Bindings;
                for (var i = 0; i < memberBindingInfos.Length; i++)
                {
                    var maInfo = memberBindingInfos[i].Expression;
                    if (!TryCollectBoundConstants(ref closure, maInfo, maInfo.NodeType, maInfo.Type, paramExprs))
                        return false;
                }
                return true;
            }
            else
            {
                var memberInitExpr = (MemberInitExpression)exprObj;
                var miNewExpr = memberInitExpr.NewExpression;
                if (!TryCollectBoundConstants(ref closure, miNewExpr, miNewExpr.NodeType, miNewExpr.Type, paramExprs))
                    return false;

                var memberBindings = memberInitExpr.Bindings;
                for (var i = 0; i < memberBindings.Count; ++i)
                {
                    var memberBinding = memberBindings[i];
                    var maExpr = ((MemberAssignment)memberBinding).Expression;
                    if (memberBinding.BindingType == MemberBindingType.Assignment &&
                        !TryCollectBoundConstants(ref closure, maExpr, maExpr.NodeType, maExpr.Type, paramExprs))
                        return false;
                }
            }

            return true;

        }

        private static bool TryCollectTryExprConstants(ref ClosureInfo closure, TryExpression tryExpr, object[] paramExprs)
        {
            if (!TryCollectBoundConstants(ref closure, tryExpr.Body, tryExpr.Body.NodeType, tryExpr.Type, paramExprs))
                return false;

            var catchBlocks = tryExpr.Handlers;
            for (var i = 0; i < catchBlocks.Count; i++)
            {
                var catchBlock = catchBlocks[i];
                var catchBody = catchBlock.Body;
                var catchExVar = catchBlock.Variable;
                if (catchExVar != null)
                {
                    closure = closure ?? new ClosureInfo();
                    closure.PushBlock(catchBody, new[] { catchExVar }, Tools.Empty<LocalBuilder>());
                    if (!TryCollectBoundConstants(ref closure, catchExVar, catchExVar.NodeType, catchBlock.Test, paramExprs))
                        return false;
                }

                var filterExpr = catchBlock.Filter;
                if (filterExpr != null &&
                    !TryCollectBoundConstants(ref closure, filterExpr, filterExpr.NodeType, filterExpr.Type, paramExprs))
                    return false;

                if (!TryCollectBoundConstants(ref closure, catchBody, catchBody.NodeType, catchBody.Type, paramExprs))
                    return false;

                if (catchExVar != null)
                    closure.PopBlock();
            }

            var finallyExpr = tryExpr.Finally;
            return finallyExpr == null
                || TryCollectBoundConstants(ref closure, finallyExpr, finallyExpr.NodeType, finallyExpr.Type, paramExprs);
        }

        private static bool TryCollectTryExprInfoConstants(ref ClosureInfo closure, TryExpressionInfo tryExpr, object[] paramExprs)
        {
            if (!TryCollectBoundConstants(ref closure, tryExpr.Body, tryExpr.Body.GetNodeType(), tryExpr.Type, paramExprs))
                return false;

            var catchBlocks = tryExpr.Handlers;
            for (var i = 0; i < catchBlocks.Length; i++)
            {
                var catchBlock = catchBlocks[i];
                var catchBody = catchBlock.Body;
                var catchExVar = catchBlock.Variable;
                if (catchExVar != null)
                {
                    closure = closure ?? new ClosureInfo();
                    closure.PushBlock(catchBody, new[] { catchExVar }, Tools.Empty<LocalBuilder>());
                    if (!TryCollectBoundConstants(ref closure, catchExVar, catchExVar.NodeType, catchBlock.Test, paramExprs))
                        return false;
                }

                var filterExpr = catchBlock.Filter;
                if (filterExpr != null &&
                    !TryCollectBoundConstants(ref closure, filterExpr, filterExpr.NodeType, filterExpr.Type, paramExprs))
                    return false;

                if (!TryCollectBoundConstants(ref closure, catchBody, catchBody.NodeType, catchBody.Type, paramExprs))
                    return false;

                if (catchExVar != null)
                    closure.PopBlock();
            }

            var finallyExpr = tryExpr.Finally;
            return finallyExpr == null
                   || TryCollectBoundConstants(ref closure, finallyExpr, finallyExpr.NodeType, finallyExpr.Type, paramExprs);
        }

        private static bool TryCollectUnaryOrBinaryExprConstants(ref ClosureInfo closure, object exprObj, object[] paramExprs)
        {
            if (exprObj is ExpressionInfo)
            {
                var unaryExprInfo = exprObj as UnaryExpressionInfo;
                if (unaryExprInfo != null)
                {
                    var opInfo = unaryExprInfo.Operand;
                    return TryCollectBoundConstants(ref closure, opInfo, opInfo.NodeType, opInfo.Type, paramExprs);
                }

                var binInfo = exprObj as BinaryExpressionInfo;
                if (binInfo != null)
                {
                    var left = binInfo.Left;
                    var right = binInfo.Right;
                    return TryCollectBoundConstants(ref closure, left, left.GetNodeType(), left.GetResultType(), paramExprs)
                           && TryCollectBoundConstants(ref closure, right, right.GetNodeType(), right.GetResultType(), paramExprs);
                }

                return false;
            }

            var unaryExpr = exprObj as UnaryExpression;
            if (unaryExpr != null)
            {
                var opExpr = unaryExpr.Operand;
                return TryCollectBoundConstants(ref closure, opExpr, opExpr.NodeType, opExpr.Type, paramExprs);
            }

            var binaryExpr = exprObj as BinaryExpression;
            if (binaryExpr != null)
            {
                var leftExpr = binaryExpr.Left;
                var rightExpr = binaryExpr.Right;
                return TryCollectBoundConstants(ref closure, leftExpr, leftExpr.NodeType, leftExpr.Type, paramExprs)
                    && TryCollectBoundConstants(ref closure, rightExpr, rightExpr.NodeType, rightExpr.Type, paramExprs);
            }

            return false;
        }

        private static bool TryCollectCallExprConstants(ref ClosureInfo closure, object exprObj, object[] paramExprs)
        {
            var callInfo = exprObj as MethodCallExpressionInfo;
            if (callInfo != null)
            {
                var objInfo = callInfo.Object;
                return (objInfo == null
                    || TryCollectBoundConstants(ref closure, objInfo, objInfo.NodeType, objInfo.Type, paramExprs))
                    && TryCollectBoundConstants(ref closure, callInfo.Arguments, paramExprs);
            }

            var callExpr = (MethodCallExpression)exprObj;
            var objExpr = callExpr.Object;
            return (objExpr == null
                    || TryCollectBoundConstants(ref closure, objExpr, objExpr.NodeType, objExpr.Type, paramExprs))
                   && TryCollectBoundConstants(ref closure, callExpr.Arguments, paramExprs);
        }

        private static KeyValuePair<ExpressionType, Type> GetExpressionMeta(object exprObj)
        {
            var expr = exprObj as Expression;
            if (expr != null)
                return new KeyValuePair<ExpressionType, Type>(expr.NodeType, expr.Type);
            var exprInfo = (ExpressionInfo)exprObj;
            return new KeyValuePair<ExpressionType, Type>(exprInfo.NodeType, exprInfo.Type);
        }

        private static bool TryCollectBoundConstants(ref ClosureInfo closure, IList<Expression> exprs, object[] paramExprs)
        {
            for (var i = 0; i < exprs.Count; i++)
            {
                var expr = exprs[i];
                if (!TryCollectBoundConstants(ref closure, expr, expr.NodeType, expr.Type, paramExprs))
                    return false;
            }
            return true;
        }

        #endregion

        /// <summary>Supports emitting of selected expressions, e.g. lambdaExpr are not supported yet.
        /// When emitter find not supported expression it will return false from <see cref="TryEmit"/>, so I could fallback
        /// to normal and slow Expression.Compile.</summary>
        private static class EmittingVisitor
        {
            private static readonly MethodInfo _getTypeFromHandleMethod = typeof(Type).GetTypeInfo()
                .DeclaredMethods.First(m => m.IsStatic && m.Name == "GetTypeFromHandle");

            private static readonly MethodInfo _objectEqualsMethod = typeof(object).GetTypeInfo()
                .DeclaredMethods.First(m => m.IsStatic && m.Name == "Equals");

            public static bool TryEmit(
                object exprObj, ExpressionType exprNodeType, Type exprType,
                object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                switch (exprNodeType)
                {
                    case ExpressionType.Parameter:
                        return EmitParameter(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.Convert:
                        return EmitConvert(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.ArrayIndex:
                        return EmitArrayIndex(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.Constant:
                        return EmitConstant(exprObj, exprType, il, closure);
                    case ExpressionType.Call:
                        return EmitMethodCall(exprObj, paramExprs, il, closure);
                    case ExpressionType.MemberAccess:
                        return EmitMemberAccess(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.New:
                        return EmitNew(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.NewArrayBounds:
                    case ExpressionType.NewArrayInit:
                        return EmitNewArray(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.MemberInit:
                        return EmitMemberInit(exprObj, exprType, paramExprs, il, closure);
                    case ExpressionType.Lambda:
                        return EmitNestedLambda(exprObj, paramExprs, il, closure);

                    case ExpressionType.Invoke:
                        return EmitInvokeLambda(exprObj, paramExprs, il, closure);

                    case ExpressionType.GreaterThan:
                    case ExpressionType.GreaterThanOrEqual:
                    case ExpressionType.LessThan:
                    case ExpressionType.LessThanOrEqual:
                    case ExpressionType.Equal:
                    case ExpressionType.NotEqual:
                        return EmitComparison((BinaryExpression)exprObj, exprNodeType, paramExprs, il, closure);

                    case ExpressionType.Add:
                    case ExpressionType.AddChecked:
                    case ExpressionType.Subtract:
                    case ExpressionType.SubtractChecked:
                    case ExpressionType.Multiply:
                    case ExpressionType.MultiplyChecked:
                    case ExpressionType.Divide:
                        return EmitArithmeticOperation(exprObj, exprType, exprNodeType, paramExprs, il, closure);

                    case ExpressionType.AndAlso:
                    case ExpressionType.OrElse:
                        return EmitLogicalOperator((BinaryExpression)exprObj, paramExprs, il, closure);

                    case ExpressionType.Coalesce:
                        return EmitCoalesceOperator((BinaryExpression)exprObj, paramExprs, il, closure);

                    case ExpressionType.Conditional:
                        return EmitConditional((ConditionalExpression)exprObj, paramExprs, il, closure);

                    case ExpressionType.Assign:
                        return EmitAssign(exprObj, exprType, paramExprs, il, closure);

                    case ExpressionType.Block:
                        return exprObj is BlockExpression ?
                            EmitBlock((BlockExpression)exprObj, paramExprs, il, closure) :
                            EmitBlockInfo((BlockExpressionInfo)exprObj, paramExprs, il, closure);

                    case ExpressionType.Try:
                        return exprObj is TryExpression
                            ? EmitTryCatchFinallyBlock((TryExpression)exprObj, exprType, paramExprs, il, closure)
                            : EmitTryCatchFinallyBlockInfo((TryExpressionInfo)exprObj, exprType, paramExprs, il, closure);

                    case ExpressionType.Throw:
                        return exprObj is UnaryExpression
                            ? EmitThrow((UnaryExpression)exprObj, paramExprs, il, closure)
                            : EmitThrowInfo((UnaryExpressionInfo)exprObj, paramExprs, il, closure);

                    case ExpressionType.Default:
                        return EmitDefault((DefaultExpression)exprObj, il);

                    case ExpressionType.Index:
                        return EmitIndex((IndexExpression)exprObj, paramExprs, il, closure);

                    default:
                        return false;
                }
            }

            private static bool EmitIndex(
                IndexExpression exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var obj = exprObj.Object;
                if (obj != null && !TryEmit(obj, obj.NodeType, obj.Type, paramExprs, il, closure))
                    return false;

                var argLength = exprObj.Arguments.Count;
                for (var i = 0; i < argLength; i++)
                {
                    var arg = exprObj.Arguments[i];
                    if (!TryEmit(arg, arg.NodeType, arg.Type, paramExprs, il, closure))
                        return false;
                }

                return EmitIndexAccess(exprObj, obj?.Type, exprObj.Type, il);
            }

            private static bool EmitCoalesceOperator(
                BinaryExpression exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var labelFalse = il.DefineLabel();
                var labelDone = il.DefineLabel();

                var left = exprObj.Left;
                var right = exprObj.Right;

                if (!TryEmit(left, left.NodeType, left.Type, paramExprs, il, closure))
                    return false;

                il.Emit(OpCodes.Dup); // duplicate left, if it's not null, after the branch this value will be on the top of the stack
                il.Emit(OpCodes.Ldnull);
                il.Emit(OpCodes.Ceq);
                il.Emit(OpCodes.Brfalse, labelFalse);

                il.Emit(OpCodes.Pop); // left is null, pop its value from the stack

                if (!TryEmit(right, right.NodeType, right.Type, paramExprs, il, closure))
                    return false;

                if (right.Type != exprObj.Type)
                    if (right.Type.GetTypeInfo().IsValueType)
                        il.Emit(OpCodes.Box, right.Type);
                    else
                        il.Emit(OpCodes.Castclass, exprObj.Type);

                il.Emit(OpCodes.Br, labelDone);

                il.MarkLabel(labelFalse);
                if (left.Type != exprObj.Type)
                    il.Emit(OpCodes.Castclass, exprObj.Type);

                il.MarkLabel(labelDone);
                return true;
            }

            private static bool EmitDefault(DefaultExpression exprObj, ILGenerator il)
            {
                var type = exprObj.Type;

                if (type == typeof(void))
                    return true;
                else if (type == typeof(string))
                    il.Emit(OpCodes.Ldnull);
                else if (type == typeof(bool) ||
                        type == typeof(byte) ||
                        type == typeof(char) ||
                        type == typeof(sbyte) ||
                        type == typeof(int) ||
                        type == typeof(uint) ||
                        type == typeof(short) ||
                        type == typeof(ushort))
                    il.Emit(OpCodes.Ldc_I4_0);
                else if (type == typeof(long) ||
                        type == typeof(ulong))
                {
                    il.Emit(OpCodes.Ldc_I4_0);
                    il.Emit(OpCodes.Conv_I8);
                }
                else if (type == typeof(float))
                    il.Emit(OpCodes.Ldc_R4, default(float));
                else if (type == typeof(double))
                    il.Emit(OpCodes.Ldc_R8, default(double));
                else if (type.GetTypeInfo().IsValueType)
                {
                    LocalBuilder lb = il.DeclareLocal(type);
                    il.Emit(OpCodes.Ldloca, lb);
                    il.Emit(OpCodes.Initobj, type);
                    il.Emit(OpCodes.Ldloc, lb);
                }
                else
                    il.Emit(OpCodes.Ldnull);

                return true;
            }

            private static bool EmitBlock(
                BlockExpression blockExpr, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                closure = closure ?? new ClosureInfo();
                closure.PushBlockAndConstructLocalVars(blockExpr.Result, blockExpr.Variables.ToArray(), il);
                var ok = EmitMany(blockExpr.Expressions, paramExprs, il, closure);
                closure.PopBlock();
                return ok;
            }

            private static bool EmitBlockInfo(
                BlockExpressionInfo blockExpr, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                closure = closure ?? new ClosureInfo();
                closure.PushBlockAndConstructLocalVars(blockExpr.Result, blockExpr.Variables, il);
                var ok = EmitMany(blockExpr.Expressions, paramExprs, il, closure);
                closure.PopBlock();
                return ok;
            }

            private static bool EmitTryCatchFinallyBlock(
                TryExpression tryExpr, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var returnLabel = default(Label);
                var returnResult = default(LocalBuilder);
                var isNonVoid = exprType != typeof(void);
                if (isNonVoid)
                {
                    returnLabel = il.DefineLabel();
                    returnResult = il.DeclareLocal(exprType);
                }

                il.BeginExceptionBlock();
                if (!TryEmit(tryExpr.Body, tryExpr.Body.NodeType, tryExpr.Body.Type, paramExprs, il, closure))
                    return false;

                if (isNonVoid)
                {
                    il.Emit(OpCodes.Stloc_S, returnResult);
                    il.Emit(OpCodes.Leave_S, returnLabel);
                }

                var catchBlocks = tryExpr.Handlers;
                for (var i = 0; i < catchBlocks.Count; i++)
                {
                    var catchBlock = catchBlocks[i];
                    if (catchBlock.Filter != null)
                        return false; // todo: Add support for filters on catch expression

                    il.BeginCatchBlock(catchBlock.Test);

                    // at the beginning of catch the Exception value is on the stack,
                    // we will store into local variable.
                    var catchBodyExpr = catchBlock.Body;
                    var exVarExpr = catchBlock.Variable;
                    if (exVarExpr != null)
                    {
                        var exVar = il.DeclareLocal(exVarExpr.Type);
                        closure = closure ?? new ClosureInfo();
                        closure.PushBlock(catchBodyExpr, new[] { exVarExpr }, new[] { exVar });
                        il.Emit(OpCodes.Stloc_S, exVar);
                    }

                    if (!TryEmit(catchBodyExpr, catchBodyExpr.NodeType, catchBodyExpr.Type, paramExprs, il, closure))
                        return false;

                    if (exVarExpr != null)
                        closure.PopBlock();

                    if (isNonVoid)
                    {
                        il.Emit(OpCodes.Stloc_S, returnResult);
                        il.Emit(OpCodes.Leave_S, returnLabel);
                    }
                    else
                        il.Emit(OpCodes.Pop);
                }

                var finallyExpr = tryExpr.Finally;
                if (finallyExpr != null)
                {
                    il.BeginFinallyBlock();
                    if (!TryEmit(finallyExpr, finallyExpr.NodeType, finallyExpr.Type, paramExprs, il, closure))
                        return false;
                }

                il.EndExceptionBlock();
                if (isNonVoid)
                {
                    il.MarkLabel(returnLabel);
                    il.Emit(OpCodes.Ldloc, returnResult);
                }

                return true;
            }

            private static bool EmitTryCatchFinallyBlockInfo(
                TryExpressionInfo tryExpr, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var returnLabel = default(Label);
                var returnResult = default(LocalBuilder);
                var isNonVoid = exprType != typeof(void);
                if (isNonVoid)
                {
                    returnLabel = il.DefineLabel();
                    returnResult = il.DeclareLocal(exprType);
                }

                il.BeginExceptionBlock();
                var bodyExpr = tryExpr.Body;
                if (!TryEmit(bodyExpr, bodyExpr.GetNodeType(), bodyExpr.GetResultType(), paramExprs, il, closure))
                    return false;

                if (isNonVoid)
                {
                    il.Emit(OpCodes.Stloc_S, returnResult);
                    il.Emit(OpCodes.Leave_S, returnLabel);
                }

                var catchBlocks = tryExpr.Handlers;
                for (var i = 0; i < catchBlocks.Length; i++)
                {
                    var catchBlock = catchBlocks[i];
                    if (catchBlock.Filter != null)
                        return false; // todo: Add support for filters on catch expression

                    il.BeginCatchBlock(catchBlock.Test);

                    // at the beginning of catch the Exception value is on the stack,
                    // we will store into local variable.
                    var catchBodyExpr = catchBlock.Body;
                    var exVarExpr = catchBlock.Variable;
                    if (exVarExpr != null)
                    {
                        var exVar = il.DeclareLocal(exVarExpr.Type);
                        closure = closure ?? new ClosureInfo();
                        closure.PushBlock(catchBodyExpr, new[] { exVarExpr }, new[] { exVar });
                        il.Emit(OpCodes.Stloc_S, exVar);
                    }

                    if (!TryEmit(catchBodyExpr, catchBodyExpr.NodeType, catchBodyExpr.Type, paramExprs, il, closure))
                        return false;

                    if (exVarExpr != null)
                        closure.PopBlock();

                    if (isNonVoid)
                    {
                        il.Emit(OpCodes.Stloc_S, returnResult);
                        il.Emit(OpCodes.Leave_S, returnLabel);
                    }
                    else
                        il.Emit(OpCodes.Pop);
                }

                var finallyExpr = tryExpr.Finally;
                if (finallyExpr != null)
                {
                    il.BeginFinallyBlock();
                    if (!TryEmit(finallyExpr, finallyExpr.NodeType, finallyExpr.Type, paramExprs, il, closure))
                        return false;
                }

                il.EndExceptionBlock();
                if (isNonVoid)
                {
                    il.MarkLabel(returnLabel);
                    il.Emit(OpCodes.Ldloc, returnResult);
                }

                return true;
            }

            private static bool EmitThrow(
                UnaryExpression exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exExpr = exprObj.Operand;
                var ok = TryEmit(exExpr, exExpr.NodeType, exExpr.Type, paramExprs, il, closure);
                il.ThrowException(exExpr.Type);
                return ok;
            }

            private static bool EmitThrowInfo(
                UnaryExpressionInfo exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exExpr = exprObj.Operand;
                var ok = TryEmit(exExpr, exExpr.NodeType, exExpr.Type, paramExprs, il, closure);
                il.ThrowException(exExpr.Type);
                return ok;
            }

            private static bool EmitParameter(
                object paramExprObj, Type paramType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                if ((paramExprObj as ParameterExpression)?.IsByRef == true) // ref, and out parameters are not supported yet
                    return false;

                // if parameter is passed, then just load it on stack
                var paramIndex = paramExprs.GetFirstIndex(paramExprObj);
                if (paramIndex != -1)
                {
                    if (closure != null && closure.HasBoundClosure)
                        paramIndex += 1; // shift parameter indices by one, because the first one will be closure
                    LoadParamArg(il, paramIndex);
                    return true;
                }

                // if parameter isn't passed, then it is passed into some outer lambda or it is a local variable,
                // so it should be loaded from closure or from the locals. Then the closure is null will be an invalid state.
                if (closure == null)
                    return false;

                // expression may represent variables as a parameters, so first look if this is the case
                var variable = closure.GetDefinedLocalVarOrDefault(paramExprObj);
                if (variable != null)
                {
                    il.Emit(OpCodes.Ldloc, variable);
                    return true;
                }

                // the only possibility that we are here is because we are in nested lambda,
                // and it uses some parameter or variable from the outer lambda
                var nonPassedParamIndex = closure.NonPassedParameters.GetFirstIndex(paramExprObj);
                if (nonPassedParamIndex == -1)
                    return false;  // what??? no chance

                var closureItemIndex = closure.Constants.Length + nonPassedParamIndex;
                LoadClosureFieldOrItem(closure, il, closureItemIndex, paramType);

                return true;
            }

            private static void LoadParamArg(ILGenerator il, int paramIndex)
            {
                // todo: consider using Ldarga_S for ValueType
                switch (paramIndex)
                {
                    case 0:
                        il.Emit(OpCodes.Ldarg_0);
                        break;
                    case 1:
                        il.Emit(OpCodes.Ldarg_1);
                        break;
                    case 2:
                        il.Emit(OpCodes.Ldarg_2);
                        break;
                    case 3:
                        il.Emit(OpCodes.Ldarg_3);
                        break;
                    default:
                        if (paramIndex <= byte.MaxValue)
                            il.Emit(OpCodes.Ldarg_S, (byte)paramIndex);
                        else
                            il.Emit(OpCodes.Ldarg, paramIndex);
                        break;
                }
            }

            private static bool EmitBinary(object exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exprInfo = exprObj as BinaryExpressionInfo;
                if (exprInfo != null)
                {
                    var left = exprInfo.Left;
                    var right = exprInfo.Right;
                    return TryEmit(left, left.GetNodeType(), left.GetResultType(), paramExprs, il, closure)
                        && TryEmit(right, right.GetNodeType(), right.GetResultType(), paramExprs, il, closure);
                }

                var expr = (BinaryExpression)exprObj;
                var leftExpr = expr.Left;
                var rightExpr = expr.Right;
                return TryEmit(leftExpr, leftExpr.NodeType, leftExpr.Type, paramExprs, il, closure)
                    && TryEmit(rightExpr, rightExpr.NodeType, rightExpr.Type, paramExprs, il, closure);
            }

            private static bool EmitMany(
                IList<Expression> exprs, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                for (int i = 0, n = exprs.Count; i < n; i++)
                {
                    var expr = exprs[i];
                    if (!TryEmit(expr, expr.NodeType, expr.Type, paramExprs, il, closure))
                        return false;
                }
                return true;
            }

            private static bool EmitMany(
                IList<object> exprObjects, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                for (int i = 0, n = exprObjects.Count; i < n; i++)
                {
                    var exprObj = exprObjects[i];
                    var exprMeta = GetExpressionMeta(exprObj);
                    if (!TryEmit(exprObj, exprMeta.Key, exprMeta.Value, paramExprs, il, closure))
                        return false;
                }
                return true;
            }

            private static bool EmitConvert(
                object exprObj, Type targetType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exprInfo = exprObj as UnaryExpressionInfo;
                Type sourceType;
                if (exprInfo != null)
                {
                    var opInfo = exprInfo.Operand;
                    if (!TryEmit(opInfo, opInfo.NodeType, opInfo.Type, paramExprs, il, closure))
                        return false;
                    sourceType = opInfo.Type;
                }
                else
                {
                    var expr = (UnaryExpression)exprObj;
                    var opExpr = expr.Operand;
                    if (!TryEmit(opExpr, opExpr.NodeType, opExpr.Type, paramExprs, il, closure))
                        return false;
                    sourceType = opExpr.Type;
                }

                if (targetType == sourceType)
                    return true; // do nothing, no conversion is needed

                if (targetType == typeof(object))
                {
                    if (sourceType.GetTypeInfo().IsValueType)
                        il.Emit(OpCodes.Box, sourceType); // for value type to object, just box a value
                }
                else if (sourceType == typeof(object) && targetType.GetTypeInfo().IsValueType)
                    il.Emit(OpCodes.Unbox_Any, targetType);

                else if (targetType.IsNullable()) // Conversion to Nullable: new Nullable<T>(T val);
                    il.Emit(OpCodes.Newobj, targetType.GetConstructorByArgs(targetType.GetWrappedTypeFromNullable()));

                else if (targetType == typeof(int))
                    il.Emit(OpCodes.Conv_I4);
                else if (targetType == typeof(float))
                    il.Emit(OpCodes.Conv_R4);
                else if (targetType == typeof(uint))
                    il.Emit(OpCodes.Conv_U4);
                else if (targetType == typeof(sbyte))
                    il.Emit(OpCodes.Conv_I1);
                else if (targetType == typeof(byte))
                    il.Emit(OpCodes.Conv_U1);
                else if (targetType == typeof(short))
                    il.Emit(OpCodes.Conv_I2);
                else if (targetType == typeof(ushort))
                    il.Emit(OpCodes.Conv_U2);
                else if (targetType == typeof(long))
                    il.Emit(OpCodes.Conv_I8);
                else if (targetType == typeof(ulong))
                    il.Emit(OpCodes.Conv_U8);
                else if (targetType == typeof(double))
                    il.Emit(OpCodes.Conv_R8);
                else
                    il.Emit(OpCodes.Castclass, targetType);

                return true;
            }

            private static bool EmitConstant(
                object exprObj, Type exprType, ILGenerator il, ClosureInfo closure)
            {
                var constExprInfo = exprObj as ConstantExpressionInfo;
                var constantValue = constExprInfo != null ? constExprInfo.Value : ((ConstantExpression)exprObj).Value;
                if (constantValue == null)
                {
                    il.Emit(OpCodes.Ldnull);
                    return true;
                }

                var constantActualType = constantValue.GetType();
                if (constantActualType.GetTypeInfo().IsEnum)
                    constantActualType = Enum.GetUnderlyingType(constantActualType);

                if (constantActualType == typeof(int))
                {
                    EmitLoadConstantInt(il, (int)constantValue);
                }
                else if (constantActualType == typeof(char))
                {
                    EmitLoadConstantInt(il, (char)constantValue);
                }
                else if (constantActualType == typeof(short))
                {
                    EmitLoadConstantInt(il, (short)constantValue);
                }
                else if (constantActualType == typeof(byte))
                {
                    EmitLoadConstantInt(il, (byte)constantValue);
                }
                else if (constantActualType == typeof(ushort))
                {
                    EmitLoadConstantInt(il, (ushort)constantValue);
                }
                else if (constantActualType == typeof(sbyte))
                {
                    EmitLoadConstantInt(il, (sbyte)constantValue);
                }
                else if (constantActualType == typeof(uint))
                {
                    unchecked
                    {
                        EmitLoadConstantInt(il, (int)(uint)constantValue);
                    }
                }
                else if (constantActualType == typeof(long))
                {
                    il.Emit(OpCodes.Ldc_I8, (long)constantValue);
                }
                else if (constantActualType == typeof(ulong))
                {
                    unchecked
                    {
                        il.Emit(OpCodes.Ldc_I8, (long)(ulong)constantValue);
                    }
                }
                else if (constantActualType == typeof(float))
                {
                    il.Emit(OpCodes.Ldc_R4, (float)constantValue);
                }
                else if (constantActualType == typeof(double))
                {
                    il.Emit(OpCodes.Ldc_R8, (double)constantValue);
                }
                else if (constantActualType == typeof(bool))
                {
                    il.Emit((bool)constantValue ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                }
                else if (constantValue is string)
                {
                    il.Emit(OpCodes.Ldstr, (string)constantValue);
                }
                else if (constantValue is Type)
                {
                    il.Emit(OpCodes.Ldtoken, (Type)constantValue);
                    il.Emit(OpCodes.Call, _getTypeFromHandleMethod);
                }
                else if (closure != null && closure.HasBoundClosure)
                {
                    var constantIndex = closure.Constants.GetFirstIndex(it => it.ConstantExpr == exprObj);
                    if (constantIndex == -1)
                        return false;
                    LoadClosureFieldOrItem(closure, il, constantIndex, exprType);
                }
                else return false;

                // todo: consider how to remove boxing where it is not required
                // boxing the value type, otherwise we can get a strange result when 0 is treated as Null.
                if (exprType == typeof(object) && constantActualType.GetTypeInfo().IsValueType)
                    il.Emit(OpCodes.Box, constantValue.GetType()); // using normal type for Enum instead of underlying type

                return true;
            }

            // if itemType is null, then itemExprObj should be not null
            private static void LoadClosureFieldOrItem(ClosureInfo closure, ILGenerator il, int itemIndex,
                Type itemType, object itemExprObj = null)
            {
                il.Emit(OpCodes.Ldarg_0); // closure is always a first argument

                // todo: consider using Ldarga for ValueType
                if (closure.Fields != null)
                    il.Emit(OpCodes.Ldfld, closure.Fields[itemIndex]);
                else
                {
                    // for ArrayClosure load an array field
                    il.Emit(OpCodes.Ldfld, ArrayClosure.ArrayField);

                    // load array item index
                    EmitLoadConstantInt(il, itemIndex);

                    // load item from index
                    il.Emit(OpCodes.Ldelem_Ref);

                    // cast or un-box the object item depending if it is a class or value type
                    itemType = itemType ?? itemExprObj.GetResultType();
                    if (itemType.GetTypeInfo().IsValueType)
                        il.Emit(OpCodes.Unbox_Any, itemType);
                    else
                        il.Emit(OpCodes.Castclass, itemType);
                }
            }

            // todo: Replace result variable with a closureInfo block
            private static bool EmitNew(
                object exprObj, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure,
                LocalBuilder resultValueVar = null)
            {
                ConstructorInfo ctor;
                var exprInfo = exprObj as NewExpressionInfo;
                if (exprInfo != null)
                {
                    if (!EmitMany(exprInfo.Arguments, paramExprs, il, closure))
                        return false;
                    ctor = exprInfo.Constructor;
                }
                else
                {
                    var expr = (NewExpression)exprObj;
                    if (!EmitMany(expr.Arguments, paramExprs, il, closure))
                        return false;
                    ctor = expr.Constructor;
                }

                if (ctor != null)
                    il.Emit(OpCodes.Newobj, ctor);
                else
                {
                    if (!exprType.GetTypeInfo().IsValueType)
                        return false; // null constructor and not a value type, better fallback

                    var valueVar = resultValueVar ?? il.DeclareLocal(exprType);
                    il.Emit(OpCodes.Ldloca, valueVar);
                    il.Emit(OpCodes.Initobj, exprType);
                    if (resultValueVar == null)
                        il.Emit(OpCodes.Ldloc, valueVar);
                }

                return true;
            }

            private static bool EmitNewArray(
                object exprObj, Type arrayType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exprInfo = exprObj as NewArrayExpressionInfo;
                if (exprInfo != null)
                    return EmitNewArrayInfo(exprInfo, arrayType, paramExprs, il, closure);

                var expr = (NewArrayExpression)exprObj;
                var elems = expr.Expressions;
                var elemType = arrayType.GetElementType();
                if (elemType == null)
                    return false;

                var isElemOfValueType = elemType.GetTypeInfo().IsValueType;

                var arrVar = il.DeclareLocal(arrayType);

                var rank = arrayType.GetArrayRank();
                if (rank == 1) // one dimensional
                {
                    EmitLoadConstantInt(il, elems.Count);
                }
                else // multi dimensional
                {
                    var boundsLength = elems.Count;
                    for (var i = 0; i < boundsLength; i++)
                    {
                        var bound = elems[i];
                        if (!TryEmit(bound, bound.NodeType, bound.Type, paramExprs, il, closure))
                            return false;
                    }

                    var constructor = arrayType.GetTypeInfo().DeclaredConstructors.GetFirst();
                    if (constructor == null) return false;
                    il.Emit(OpCodes.Newobj, constructor);

                    return true;
                }

                il.Emit(OpCodes.Newarr, elemType);
                il.Emit(OpCodes.Stloc, arrVar);

                for (int i = 0, n = elems.Count; i < n; i++)
                {
                    il.Emit(OpCodes.Ldloc, arrVar);
                    EmitLoadConstantInt(il, i);

                    // loading element address for later copying of value into it.
                    if (isElemOfValueType)
                        il.Emit(OpCodes.Ldelema, elemType);

                    var elemExpr = elems[i];
                    if (!TryEmit(elemExpr, elemExpr.NodeType, elemExpr.Type, paramExprs, il, closure))
                        return false;

                    if (isElemOfValueType)
                        il.Emit(OpCodes.Stobj, elemType); // store element of value type by array element address
                    else
                        il.Emit(OpCodes.Stelem_Ref);
                }

                il.Emit(OpCodes.Ldloc, arrVar);
                return true;
            }

            private static bool EmitNewArrayInfo(
                NewArrayExpressionInfo expr, Type arrayType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var elemExprObjects = expr.Arguments;
                var elemType = arrayType.GetElementType();
                if (elemType == null)
                    return false;

                var isElemOfValueType = elemType.GetTypeInfo().IsValueType;

                var arrVar = il.DeclareLocal(arrayType);

                EmitLoadConstantInt(il, elemExprObjects.Length);
                il.Emit(OpCodes.Newarr, elemType);
                il.Emit(OpCodes.Stloc, arrVar);

                for (var i = 0; i < elemExprObjects.Length; i++)
                {
                    il.Emit(OpCodes.Ldloc, arrVar);
                    EmitLoadConstantInt(il, i);

                    // loading element address for later copying of value into it.
                    if (isElemOfValueType)
                        il.Emit(OpCodes.Ldelema, elemType);

                    var elemExprObject = elemExprObjects[i];
                    var elemExprMeta = GetExpressionMeta(elemExprObject);
                    if (!TryEmit(elemExprObject, elemExprMeta.Key, elemExprMeta.Value, paramExprs, il, closure))
                        return false;

                    if (isElemOfValueType)
                        il.Emit(OpCodes.Stobj, elemType); // store element of value type by array element address
                    else
                        il.Emit(OpCodes.Stelem_Ref);
                }

                il.Emit(OpCodes.Ldloc, arrVar);
                return true;
            }

            private static bool EmitArrayIndex(object exprObj, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                if (!EmitBinary(exprObj, paramExprs, il, closure))
                    return false;
                if (exprType.GetTypeInfo().IsValueType)
                    il.Emit(OpCodes.Ldelem, exprType);
                else
                    il.Emit(OpCodes.Ldelem_Ref);
                return true;
            }

            private static bool EmitMemberInit(
                object exprObj, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exprInfo = exprObj as MemberInitExpressionInfo;
                if (exprInfo != null)
                    return EmitMemberInitInfo(exprInfo, exprType, paramExprs, il, closure);

                // todo: Use closureInfo Block to track the variable instead
                LocalBuilder valueVar = null;
                if (exprType.GetTypeInfo().IsValueType)
                    valueVar = il.DeclareLocal(exprType);

                var expr = (MemberInitExpression)exprObj;
                if (!EmitNew(expr.NewExpression, exprType, paramExprs, il, closure, valueVar))
                    return false;

                var bindings = expr.Bindings;
                for (var i = 0; i < bindings.Count; i++)
                {
                    var binding = bindings[i];
                    if (binding.BindingType != MemberBindingType.Assignment)
                        return false;

                    if (valueVar != null) // load local value address, to set its members
                        il.Emit(OpCodes.Ldloca, valueVar);
                    else
                        il.Emit(OpCodes.Dup); // duplicate member owner on stack

                    var bindingExpr = ((MemberAssignment)binding).Expression;
                    if (!TryEmit(bindingExpr, bindingExpr.NodeType, bindingExpr.Type, paramExprs, il, closure) ||
                        !EmitMemberAssign(il, binding.Member))
                        return false;
                }

                if (valueVar != null)
                    il.Emit(OpCodes.Ldloc, valueVar);

                return true;
            }

            private static bool EmitMemberInitInfo(
                MemberInitExpressionInfo exprInfo, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                LocalBuilder valueVar = null;
                if (exprType.GetTypeInfo().IsValueType)
                    valueVar = il.DeclareLocal(exprType);

                var objInfo = exprInfo.ExpressionInfo;
                if (objInfo == null)
                    return false; // static initialization is Not supported

                var newExpr = exprInfo.NewExpressionInfo;
                if (newExpr != null)
                {
                    if (!EmitNew(newExpr, exprType, paramExprs, il, closure, valueVar))
                        return false;
                }
                else
                {
                    if (!TryEmit(objInfo, objInfo.NodeType, objInfo.Type, paramExprs, il, closure))
                        return false;
                }

                var bindings = exprInfo.Bindings;
                for (var i = 0; i < bindings.Length; i++)
                {
                    var binding = bindings[i];

                    if (valueVar != null) // load local value address, to set its members
                        il.Emit(OpCodes.Ldloca, valueVar);
                    else
                        il.Emit(OpCodes.Dup); // duplicate member owner on stack

                    var bindingExpr = binding.Expression;
                    if (!TryEmit(bindingExpr, bindingExpr.NodeType, bindingExpr.Type, paramExprs, il, closure) ||
                        !EmitMemberAssign(il, binding.Member))
                        return false;
                }

                if (valueVar != null)
                    il.Emit(OpCodes.Ldloc, valueVar);

                return true;
            }

            private static bool EmitMemberAssign(ILGenerator il, MemberInfo member)
            {
                var prop = member as PropertyInfo;
                if (prop != null)
                {
                    var propSetMethodName = "set_" + prop.Name;
                    var setMethod = prop.DeclaringType.GetTypeInfo()
                        .DeclaredMethods.GetFirst(m => m.Name == propSetMethodName);
                    if (setMethod == null)
                        return false;
                    EmitMethodCall(il, setMethod);
                }
                else
                {
                    var field = member as FieldInfo;
                    if (field == null)
                        return false;
                    il.Emit(OpCodes.Stfld, field);
                }
                return true;
            }

            private static bool EmitAssign(
                object exprObj, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                object left, right;
                ExpressionType leftNodeType, rightNodeType;

                var expr = exprObj as BinaryExpression;
                if (expr != null)
                {
                    left = expr.Left;
                    right = expr.Right;
                    leftNodeType = expr.Left.NodeType;
                    rightNodeType = expr.Right.NodeType;
                }
                else
                {
                    var info = (BinaryExpressionInfo)exprObj;
                    left = info.Left;
                    right = info.Right;
                    leftNodeType = left.GetNodeType();
                    rightNodeType = right.GetNodeType();
                }

                // if this assignment is part of a single body-less expression or the result of a block
                // we should put its result to the evaluation stack before the return, otherwise we are
                // somewhere inside the block, so we shouldn't return with the result
                var shouldPushResult = closure == null
                    || closure.CurrentBlock.IsEmpty
                    || closure.CurrentBlock.ResultExpr == exprObj;

                switch (leftNodeType)
                {
                    case ExpressionType.Parameter:
                        var paramIndex = paramExprs.GetFirstIndex(left);
                        if (paramIndex != -1)
                        {
                            if (closure != null && closure.HasBoundClosure)
                                paramIndex += 1; // shift parameter indices by one, because the first one will be closure

                            if (paramIndex >= byte.MaxValue)
                                return false;

                            if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                                return false;

                            if (shouldPushResult)
                                il.Emit(OpCodes.Dup); // dup value to assign and return

                            il.Emit(OpCodes.Starg_S, paramIndex);
                            return true;
                        }

                        // if parameter isn't passed, then it is passed into some outer lambda or it is a local variable,
                        // so it should be loaded from closure or from the locals. Then the closure is null will be an invalid state.
                        if (closure == null)
                            return false;

                        // if it's a local variable, then store the right value in it
                        var localVariable = closure.GetDefinedLocalVarOrDefault(left);
                        if (localVariable != null)
                        {
                            if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                                return false;

                            if (shouldPushResult) // if we have to push the result back, dup the right value
                                il.Emit(OpCodes.Dup);

                            il.Emit(OpCodes.Stloc, localVariable);
                            return true;
                        }

                        // check that it's a captured parameter by closure
                        var nonPassedParamIndex = closure.NonPassedParameters.GetFirstIndex(left);
                        if (nonPassedParamIndex == -1)
                            return false;  // what??? no chance

                        var paramInClosureIndex = closure.Constants.Length + nonPassedParamIndex;

                        il.Emit(OpCodes.Ldarg_0); // closure is always a first argument

                        if (shouldPushResult)
                        {
                            if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                                return false;

                            var valueVar = il.DeclareLocal(exprType); // store left value in variable
                            if (closure.Fields != null)
                            {
                                il.Emit(OpCodes.Dup);
                                il.Emit(OpCodes.Stloc, valueVar);
                                il.Emit(OpCodes.Stfld, closure.Fields[paramInClosureIndex]);
                                il.Emit(OpCodes.Ldloc, valueVar);
                            }
                            else
                            {
                                il.Emit(OpCodes.Stloc, valueVar);
                                il.Emit(OpCodes.Ldfld, ArrayClosure.ArrayField); // load array field
                                EmitLoadConstantInt(il, paramInClosureIndex); // load array item index
                                il.Emit(OpCodes.Ldloc, valueVar);
                                if (exprType.GetTypeInfo().IsValueType)
                                    il.Emit(OpCodes.Box, exprType);
                                il.Emit(OpCodes.Stelem_Ref); // put the variable into array
                                il.Emit(OpCodes.Ldloc, valueVar);
                            }
                        }
                        else
                        {
                            var isArrayClosure = closure.Fields == null;
                            if (isArrayClosure)
                            {
                                il.Emit(OpCodes.Ldfld, ArrayClosure.ArrayField); // load array field
                                EmitLoadConstantInt(il, paramInClosureIndex); // load array item index
                            }

                            if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                                return false;

                            if (isArrayClosure)
                            {
                                if (exprType.GetTypeInfo().IsValueType)
                                    il.Emit(OpCodes.Box, exprType);
                                il.Emit(OpCodes.Stelem_Ref); // put the variable into array
                            }
                            else
                                il.Emit(OpCodes.Stfld, closure.Fields[paramInClosureIndex]);
                        }
                        return true;

                    case ExpressionType.MemberAccess:

                        object objExpr;
                        MemberInfo member;

                        var memberExpr = left as MemberExpression;
                        if (memberExpr != null)
                        {
                            objExpr = memberExpr.Expression;
                            member = memberExpr.Member;
                        }
                        else
                        {
                            var memberExprInfo = (MemberExpressionInfo)left;
                            objExpr = memberExprInfo.Expression;
                            member = memberExprInfo.Member;
                        }

                        if (objExpr != null &&
                            !TryEmit(objExpr, objExpr.GetNodeType(), objExpr.GetResultType(), paramExprs, il, closure))
                            return false;

                        if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                            return false;

                        if (!shouldPushResult)
                            return EmitMemberAssign(il, member);

                        il.Emit(OpCodes.Dup);

                        var rightVar = il.DeclareLocal(exprType); // store right value in variable
                        il.Emit(OpCodes.Stloc, rightVar);

                        if (!EmitMemberAssign(il, member))
                            return false;

                        il.Emit(OpCodes.Ldloc, rightVar);
                        return true;

                    case ExpressionType.Index:
                        var indexExpr = (IndexExpression)left;

                        var obj = indexExpr.Object;
                        if (obj != null && !TryEmit(obj, obj.NodeType, obj.Type, paramExprs, il, closure))
                            return false;

                        var argLength = indexExpr.Arguments.Count;
                        for (var i = 0; i < argLength; i++)
                        {
                            var arg = indexExpr.Arguments[i];
                            if (!TryEmit(arg, arg.NodeType, arg.Type, paramExprs, il, closure))
                                return false;
                        }

                        if (!TryEmit(right, rightNodeType, exprType, paramExprs, il, closure))
                            return false;

                        if (!shouldPushResult)
                            return EmitIndexAssign(indexExpr, obj?.Type, exprType, il);

                        var variable = il.DeclareLocal(exprType); // store value in variable to return
                        il.Emit(OpCodes.Dup);
                        il.Emit(OpCodes.Stloc, variable);

                        if (!EmitIndexAssign(indexExpr, obj?.Type, exprType, il))
                            return false;

                        il.Emit(OpCodes.Ldloc, variable);

                        return true;

                    default: // not yet support assignment targets
                        return false;
                }
            }

            private static bool EmitIndexAssign(IndexExpression indexExpr, Type instType, Type elementType, ILGenerator il)
            {
                if (indexExpr.Indexer != null)
                    return EmitMemberAssign(il, indexExpr.Indexer);

                if (indexExpr.Arguments.Count == 1) // one dimensional array
                {
                    if (elementType.GetTypeInfo().IsValueType)
                        il.Emit(OpCodes.Stelem, elementType);
                    else
                        il.Emit(OpCodes.Stelem_Ref);
                }
                else // multi dimensional array
                {
                    if (instType == null)
                        return false;

                    var setMethod = instType.GetTypeInfo().GetDeclaredMethod("Set");
                    EmitMethodCall(il, setMethod);
                }

                return true;
            }

            private static bool EmitIndexAccess(IndexExpression indexExpr, Type instType, Type elementType, ILGenerator il)
            {
                if (indexExpr.Indexer != null)
                    return EmitMemberAccess(il, indexExpr.Indexer);

                if (indexExpr.Arguments.Count == 1) // one dimensional array
                {
                    if (elementType.GetTypeInfo().IsValueType)
                        il.Emit(OpCodes.Ldelem, elementType);
                    else
                        il.Emit(OpCodes.Ldelem_Ref);
                }
                else // multi dimensional array
                {
                    if (instType == null)
                        return false;

                    var setMethod = instType.GetTypeInfo().GetDeclaredMethod("Get");
                    EmitMethodCall(il, setMethod);
                }

                return true;
            }

            private static bool EmitMethodCall(
                object exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var exprInfo = exprObj as MethodCallExpressionInfo;
                if (exprInfo != null)
                {
                    var objInfo = exprInfo.Object;
                    if (objInfo != null)
                    {
                        var objType = objInfo.Type;
                        if (!TryEmit(objInfo, objInfo.NodeType, objType, paramExprs, il, closure))
                            return false;
                        if (objType.GetTypeInfo().IsValueType)
                            il.Emit(OpCodes.Box, objType); // todo: not optimal, should be replaced by Ldloca
                    }

                    if (exprInfo.Arguments.Length != 0 &&
                        !EmitMany(exprInfo.Arguments, paramExprs, il, closure))
                        return false;
                }
                else
                {
                    var expr = (MethodCallExpression)exprObj;
                    var objExpr = expr.Object;
                    if (objExpr != null)
                    {
                        var objType = objExpr.Type;
                        if (!TryEmit(objExpr, objExpr.NodeType, objType, paramExprs, il, closure))
                            return false;
                        if (objType.GetTypeInfo().IsValueType)
                            il.Emit(OpCodes.Box, objType); // todo: not optimal, should be replaced by Ldloca
                    }

                    if (expr.Arguments.Count != 0 &&
                        !EmitMany(expr.Arguments, paramExprs, il, closure))
                        return false;
                }

                var method = exprInfo != null ? exprInfo.Method : ((MethodCallExpression)exprObj).Method;
                EmitMethodCall(il, method);
                return true;
            }

            private static bool EmitMemberAccess(
                object exprObj, Type exprType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                MemberInfo member;
                Type instanceType = null;
                var exprInfo = exprObj as MemberExpressionInfo;
                if (exprInfo != null)
                {
                    var instance = exprInfo.Expression;
                    if (instance != null)
                    {
                        instanceType = instance.GetResultType();
                        if (!TryEmit(instance, instance.GetNodeType(), instanceType, paramExprs, il, closure))
                            return false;
                    }
                    member = exprInfo.Member;
                }
                else
                {
                    var expr = (MemberExpression)exprObj;
                    var instExpr = expr.Expression;
                    if (instExpr != null)
                    {
                        instanceType = instExpr.Type;
                        if (!TryEmit(instExpr, instExpr.NodeType, instanceType, paramExprs, il, closure))
                            return false;
                    }
                    member = expr.Member;
                }

                if (instanceType != null) // it is a non-static member access
                {
                    // value type special treatment to load address of value instance
                    // in order to access value member or call a method (does a copy).
                    // todo: May be optimized for method call to load address of initial variable without copy
                    if (instanceType.GetTypeInfo().IsValueType)
                    {
                        if (exprType.GetTypeInfo().IsValueType ||
                            member is PropertyInfo)
                        {
                            var valueVar = il.DeclareLocal(instanceType);
                            il.Emit(OpCodes.Stloc, valueVar);
                            il.Emit(OpCodes.Ldloca, valueVar);
                        }
                    }
                }

                return EmitMemberAccess(il, member);
            }

            private static bool EmitMemberAccess(ILGenerator il, MemberInfo member)
            {
                var field = member as FieldInfo;
                if (field != null)
                {
                    il.Emit(field.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, field);
                    return true;
                }

                var prop = member as PropertyInfo;
                if (prop != null)
                {
                    var getMethod = TryGetPropertyGetMethod(prop);
                    if (getMethod == null)
                        return false;
                    EmitMethodCall(il, getMethod);
                    return true;
                }

                return false;
            }

            private static MethodInfo TryGetPropertyGetMethod(PropertyInfo prop)
            {
                var propGetMethodName = "get_" + prop.Name;
                var getMethod = prop.DeclaringType.GetTypeInfo()
                    .DeclaredMethods.GetFirst(m => m.Name == propGetMethodName);
                return getMethod;
            }

            private static bool EmitNestedLambda(
                object lambdaExpr, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                // First, find in closed compiled lambdas the one corresponding to the current lambda expression.
                // Situation with not found lambda is not possible/exceptional,
                // it means that we somehow skipped the lambda expression while collecting closure info.
                var outerNestedLambdas = closure.NestedLambdas;
                var outerNestedLambdaIndex = outerNestedLambdas.GetFirstIndex(it => it.LambdaExpr == lambdaExpr);
                if (outerNestedLambdaIndex == -1)
                    return false;

                var nestedLambdaInfo = outerNestedLambdas[outerNestedLambdaIndex];
                var nestedLambda = nestedLambdaInfo.Lambda;

                var outerConstants = closure.Constants;
                var outerNonPassedParams = closure.NonPassedParameters;

                // Load compiled lambda on stack counting the offset
                outerNestedLambdaIndex += outerConstants.Length + outerNonPassedParams.Length;

                LoadClosureFieldOrItem(closure, il, outerNestedLambdaIndex, nestedLambda.GetType());

                // If lambda does not use any outer parameters to be set in closure, then we're done
                var nestedClosureInfo = nestedLambdaInfo.ClosureInfo;
                if (nestedClosureInfo == null)
                    return true;

                // If closure is array-based, the create a new array to represent closure for the nested lambda
                var isNestedArrayClosure = nestedClosureInfo.Fields == null;
                if (isNestedArrayClosure)
                {
                    EmitLoadConstantInt(il, nestedClosureInfo.ClosedItemCount); // size of array
                    il.Emit(OpCodes.Newarr, typeof(object));
                }

                // Load constants on stack
                var nestedConstants = nestedClosureInfo.Constants;
                if (nestedConstants.Length != 0)
                {
                    for (var nestedConstIndex = 0; nestedConstIndex < nestedConstants.Length; nestedConstIndex++)
                    {
                        var nestedConstant = nestedConstants[nestedConstIndex];

                        // Find constant index in the outer closure
                        var outerConstIndex = outerConstants.GetFirstIndex(it => it.ConstantExpr == nestedConstant.ConstantExpr);
                        if (outerConstIndex == -1)
                            return false; // some error is here

                        if (isNestedArrayClosure)
                        {
                            // Duplicate nested array on stack to store the item, and load index to where to store
                            il.Emit(OpCodes.Dup);
                            EmitLoadConstantInt(il, nestedConstIndex);
                        }

                        LoadClosureFieldOrItem(closure, il, outerConstIndex, nestedConstant.Type);

                        if (isNestedArrayClosure)
                        {
                            if (nestedConstant.Type.GetTypeInfo().IsValueType)
                                il.Emit(OpCodes.Box, nestedConstant.Type);
                            il.Emit(OpCodes.Stelem_Ref); // store the item in array
                        }
                    }
                }

                // Load used and closed parameter values on stack
                var nestedNonPassedParams = nestedClosureInfo.NonPassedParameters;
                for (var nestedParamIndex = 0; nestedParamIndex < nestedNonPassedParams.Length; nestedParamIndex++)
                {
                    var nestedUsedParam = nestedNonPassedParams[nestedParamIndex];

                    Type nestedUsedParamType = null;
                    if (isNestedArrayClosure)
                    {
                        // get a param type for the later
                        nestedUsedParamType = nestedUsedParam.GetResultType();

                        // Duplicate nested array on stack to store the item, and load index to where to store
                        il.Emit(OpCodes.Dup);
                        EmitLoadConstantInt(il, nestedConstants.Length + nestedParamIndex);
                    }

                    var paramIndex = paramExprs.GetFirstIndex(nestedUsedParam);
                    if (paramIndex != -1) // load param from input params
                    {
                        // +1 is set cause of added first closure argument
                        LoadParamArg(il, 1 + paramIndex);
                    }
                    else // load parameter from outer closure or from the locals
                    {
                        if (outerNonPassedParams.Length == 0)
                            return false; // impossible, better to throw?

                        var variable = closure.GetDefinedLocalVarOrDefault(nestedUsedParam);
                        if (variable != null) // it's a local variable
                        {
                            il.Emit(OpCodes.Ldloc, variable);
                        }
                        else // it's a parameter from outer closure
                        {
                            var outerParamIndex = outerNonPassedParams.GetFirstIndex(nestedUsedParam);
                            if (outerParamIndex == -1)
                                return false; // impossible, better to throw?

                            LoadClosureFieldOrItem(closure, il, outerConstants.Length + outerParamIndex,
                                nestedUsedParamType, nestedUsedParam);
                        }
                    }

                    if (isNestedArrayClosure)
                    {
                        if (nestedUsedParamType.GetTypeInfo().IsValueType)
                            il.Emit(OpCodes.Box, nestedUsedParamType);

                        il.Emit(OpCodes.Stelem_Ref); // store the item in array
                    }
                }

                // Load nested lambdas on stack
                var nestedNestedLambdas = nestedClosureInfo.NestedLambdas;
                if (nestedNestedLambdas.Length != 0)
                {
                    for (var nestedLambdaIndex = 0; nestedLambdaIndex < nestedNestedLambdas.Length; nestedLambdaIndex++)
                    {
                        var nestedNestedLambda = nestedNestedLambdas[nestedLambdaIndex];

                        // Find constant index in the outer closure
                        var outerLambdaIndex = outerNestedLambdas.GetFirstIndex(it => it.LambdaExpr == nestedNestedLambda.LambdaExpr);
                        if (outerLambdaIndex == -1)
                            return false; // some error is here

                        // Duplicate nested array on stack to store the item, and load index to where to store
                        if (isNestedArrayClosure)
                        {
                            il.Emit(OpCodes.Dup);
                            EmitLoadConstantInt(il, nestedConstants.Length + nestedNonPassedParams.Length + nestedLambdaIndex);
                        }

                        outerLambdaIndex += outerConstants.Length + outerNonPassedParams.Length;

                        LoadClosureFieldOrItem(closure, il, outerLambdaIndex, nestedNestedLambda.Lambda.GetType());

                        if (isNestedArrayClosure)
                            il.Emit(OpCodes.Stelem_Ref); // store the item in array
                    }
                }

                // Create nested closure object composed of all constants, params, lambdas loaded on stack
                if (isNestedArrayClosure)
                    il.Emit(OpCodes.Newobj, ArrayClosure.Constructor);
                else
                    il.Emit(OpCodes.Newobj,
                        nestedClosureInfo.ClosureType.GetTypeInfo().DeclaredConstructors.GetFirst());

                EmitMethodCall(il, GetCurryClosureMethod(nestedLambda, nestedLambdaInfo.IsAction));
                return true;
            }

            private static MethodInfo GetCurryClosureMethod(object lambda, bool isAction)
            {
                var lambdaTypeArgs = lambda.GetType().GetTypeInfo().GenericTypeArguments;
                return isAction
                    ? CurryClosureActions.Methods[lambdaTypeArgs.Length - 1].MakeGenericMethod(lambdaTypeArgs)
                    : CurryClosureFuncs.Methods[lambdaTypeArgs.Length - 2].MakeGenericMethod(lambdaTypeArgs);
            }

            private static bool EmitInvokeLambda(
                object exprObj, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var expr = exprObj as InvocationExpression;
                Type lambdaType;
                if (expr != null)
                {
                    var lambdaExpr = expr.Expression;
                    lambdaType = lambdaExpr.Type;
                    if (!TryEmit(lambdaExpr, lambdaExpr.NodeType, lambdaType, paramExprs, il, closure) ||
                        !EmitMany(expr.Arguments, paramExprs, il, closure))
                        return false;
                }
                else
                {
                    var exprInfo = (InvocationExpressionInfo)exprObj;
                    var lambdaExprInfo = exprInfo.ExprToInvoke;
                    lambdaType = lambdaExprInfo.Type;
                    if (!TryEmit(lambdaExprInfo, lambdaExprInfo.NodeType, lambdaType, paramExprs, il, closure) ||
                        !EmitMany(exprInfo.Arguments, paramExprs, il, closure))
                        return false;
                }

                var invokeMethod = lambdaType.GetTypeInfo().DeclaredMethods.GetFirst(m => m.Name == "Invoke");
                EmitMethodCall(il, invokeMethod);
                return true;
            }

            private static bool EmitComparison(
                BinaryExpression expr, ExpressionType exprNodeType,
                object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                if (!EmitBinary(expr, paramExprs, il, closure))
                    return false;

                var leftOpType = expr.Left.Type;
                var leftOpTypeInfo = leftOpType.GetTypeInfo();
                if (!leftOpTypeInfo.IsPrimitive &&
                    !leftOpTypeInfo.IsEnum)
                {
                    var methodName
                        = exprNodeType == ExpressionType.Equal ? "op_Equality"
                        : exprNodeType == ExpressionType.NotEqual ? "op_Inequality"
                        : exprNodeType == ExpressionType.GreaterThan ? "op_GreaterThan"
                        : exprNodeType == ExpressionType.GreaterThanOrEqual ? "op_GreaterThanOrEqual"
                        : exprNodeType == ExpressionType.LessThan ? "op_LessThan"
                        : exprNodeType == ExpressionType.LessThanOrEqual ? "op_LessThanOrEqual" :
                        null;

                    if (methodName == null)
                        return false;

                    // todo: for now handling only parameters of the same type
                    var method = leftOpTypeInfo.DeclaredMethods.GetFirst(m =>
                        m.IsStatic && m.Name == methodName &&
                        m.GetParameters().All(p => p.ParameterType == leftOpType));

                    if (method != null)
                    {
                        EmitMethodCall(il, method);
                    }
                    else
                    {
                        if (exprNodeType != ExpressionType.Equal && exprNodeType != ExpressionType.NotEqual)
                            return false;

                        EmitMethodCall(il, _objectEqualsMethod);
                        if (exprNodeType == ExpressionType.NotEqual) // add not to equal
                        {
                            il.Emit(OpCodes.Ldc_I4_0);
                            il.Emit(OpCodes.Ceq);
                        }
                    }

                    return true;
                }

                // emit for primitives
                switch (exprNodeType)
                {
                    case ExpressionType.Equal:
                        il.Emit(OpCodes.Ceq);
                        return true;

                    case ExpressionType.NotEqual:
                        il.Emit(OpCodes.Ceq);
                        il.Emit(OpCodes.Ldc_I4_0);
                        il.Emit(OpCodes.Ceq);
                        return true;

                    case ExpressionType.LessThan:
                        il.Emit(OpCodes.Clt);
                        return true;

                    case ExpressionType.GreaterThan:
                        il.Emit(OpCodes.Cgt);
                        return true;

                    case ExpressionType.LessThanOrEqual:
                        il.Emit(OpCodes.Cgt);
                        il.Emit(OpCodes.Ldc_I4_0);
                        il.Emit(OpCodes.Ceq);
                        return true;

                    case ExpressionType.GreaterThanOrEqual:
                        il.Emit(OpCodes.Clt);
                        il.Emit(OpCodes.Ldc_I4_0);
                        il.Emit(OpCodes.Ceq);
                        return true;
                }
                return false;
            }

            private static bool EmitArithmeticOperation(
                object exprObj, Type exprType, ExpressionType exprNodeType, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                if (!EmitBinary(exprObj, paramExprs, il, closure))
                    return false;

                var exprTypeInfo = exprType.GetTypeInfo();
                if (!exprTypeInfo.IsPrimitive)
                {
                    var methodName
                        = exprNodeType == ExpressionType.Add ? "op_Addition"
                        : exprNodeType == ExpressionType.AddChecked ? "op_Addition"
                        : exprNodeType == ExpressionType.Subtract ? "op_Subtraction"
                        : exprNodeType == ExpressionType.SubtractChecked ? "op_Subtraction"
                        : exprNodeType == ExpressionType.Multiply ? "op_Multiply"
                        : exprNodeType == ExpressionType.Divide ? "op_Division"
                        : null;

                    if (methodName == null)
                        return false;

                    EmitMethodCall(il, exprTypeInfo.GetDeclaredMethod(methodName));
                    return true;
                }

                switch (exprNodeType)
                {
                    case ExpressionType.Add:
                        il.Emit(OpCodes.Add);
                        return true;

                    case ExpressionType.AddChecked:
                        il.Emit(IsUnsigned(exprType) ? OpCodes.Add_Ovf_Un : OpCodes.Add_Ovf);
                        return true;

                    case ExpressionType.Subtract:
                        il.Emit(OpCodes.Sub);
                        return true;

                    case ExpressionType.SubtractChecked:
                        il.Emit(IsUnsigned(exprType) ? OpCodes.Sub_Ovf_Un : OpCodes.Sub_Ovf);
                        return true;

                    case ExpressionType.Multiply:
                        il.Emit(OpCodes.Mul);
                        return true;

                    case ExpressionType.MultiplyChecked:
                        il.Emit(IsUnsigned(exprType) ? OpCodes.Mul_Ovf_Un : OpCodes.Mul_Ovf);
                        return true;

                    case ExpressionType.Divide:
                        il.Emit(OpCodes.Div);
                        return true;
                }

                return false;
            }

            private static bool IsUnsigned(Type type) =>
                type == typeof(byte) || type == typeof(ushort) || type == typeof(uint) || type == typeof(ulong);

            private static bool EmitLogicalOperator(
                BinaryExpression expr, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var leftExpr = expr.Left;
                if (!TryEmit(leftExpr, leftExpr.NodeType, leftExpr.Type, paramExprs, il, closure))
                    return false;

                var labelSkipRight = il.DefineLabel();
                var isAnd = expr.NodeType == ExpressionType.AndAlso;
                il.Emit(isAnd ? OpCodes.Brfalse : OpCodes.Brtrue, labelSkipRight);

                var rightExpr = expr.Right;
                if (!TryEmit(rightExpr, rightExpr.NodeType, rightExpr.Type, paramExprs, il, closure))
                    return false;

                var labelDone = il.DefineLabel();
                il.Emit(OpCodes.Br, labelDone);

                il.MarkLabel(labelSkipRight); // label the second branch
                il.Emit(isAnd ? OpCodes.Ldc_I4_0 : OpCodes.Ldc_I4_1);
                il.MarkLabel(labelDone);
                return true;
            }

            private static bool EmitConditional(
                ConditionalExpression expr, object[] paramExprs, ILGenerator il, ClosureInfo closure)
            {
                var testExpr = expr.Test;
                if (!TryEmit(testExpr, testExpr.NodeType, testExpr.Type, paramExprs, il, closure))
                    return false;

                var labelIfFalse = il.DefineLabel();
                il.Emit(OpCodes.Brfalse, labelIfFalse);

                var ifTrueExpr = expr.IfTrue;
                if (!TryEmit(ifTrueExpr, ifTrueExpr.NodeType, ifTrueExpr.Type, paramExprs, il, closure))
                    return false;

                var labelDone = il.DefineLabel();
                il.Emit(OpCodes.Br, labelDone);

                il.MarkLabel(labelIfFalse);
                var ifFalseExpr = expr.IfFalse;
                if (!TryEmit(ifFalseExpr, ifFalseExpr.NodeType, ifFalseExpr.Type, paramExprs, il, closure))
                    return false;

                il.MarkLabel(labelDone);
                return true;
            }

            private static void EmitMethodCall(ILGenerator il, MethodInfo method)
            {
                il.Emit(method.IsVirtual ? OpCodes.Callvirt : OpCodes.Call, method);
            }

            private static void EmitLoadConstantInt(ILGenerator il, int i)
            {
                switch (i)
                {
                    case -1:
                        il.Emit(OpCodes.Ldc_I4_M1);
                        break;
                    case 0:
                        il.Emit(OpCodes.Ldc_I4_0);
                        break;
                    case 1:
                        il.Emit(OpCodes.Ldc_I4_1);
                        break;
                    case 2:
                        il.Emit(OpCodes.Ldc_I4_2);
                        break;
                    case 3:
                        il.Emit(OpCodes.Ldc_I4_3);
                        break;
                    case 4:
                        il.Emit(OpCodes.Ldc_I4_4);
                        break;
                    case 5:
                        il.Emit(OpCodes.Ldc_I4_5);
                        break;
                    case 6:
                        il.Emit(OpCodes.Ldc_I4_6);
                        break;
                    case 7:
                        il.Emit(OpCodes.Ldc_I4_7);
                        break;
                    case 8:
                        il.Emit(OpCodes.Ldc_I4_8);
                        break;
                    default:
                        il.Emit(OpCodes.Ldc_I4, i);
                        break;
                }
            }
        }
    }

    // Helpers targeting the performance.
    // Extensions method names may be a bit funny (non standard), 
    // it is done to prevent conflicts with helpers with standard names
    internal static class Tools
    {
        public static bool IsNullable(this Type type) =>
            type.GetTypeInfo().IsGenericType && type.GetTypeInfo().GetGenericTypeDefinition() == typeof(Nullable<>);

        public static Type GetWrappedTypeFromNullable(this Type type) =>
            type.GetTypeInfo().GenericTypeArguments[0];

        public static ConstructorInfo GetConstructorByArgs(this Type type, params Type[] args) =>
            type.GetTypeInfo().DeclaredConstructors.GetFirst(c => c.GetParameters().Project(p => p.ParameterType).SequenceEqual(args));

        public static Expression ToExpression(this object exprObj) =>
            exprObj == null ? null : exprObj as Expression ?? ((ExpressionInfo)exprObj).ToExpression();

        public static ExpressionType GetNodeType(this object exprObj) =>
            (exprObj as Expression)?.NodeType ?? ((ExpressionInfo)exprObj).NodeType;

        public static Type GetResultType(this object exprObj) =>
            (exprObj as Expression)?.Type ?? ((ExpressionInfo)exprObj).Type;

        private static class EmptyArray<T>
        {
            public static readonly T[] Value = new T[0];
        }

        public static T[] Empty<T>() => EmptyArray<T>.Value;

        public static T[] WithLast<T>(this T[] source, T value)
        {
            if (source == null || source.Length == 0)
                return new[] { value };
            if (source.Length == 1)
                return new[] { source[0], value };
            if (source.Length == 2)
                return new[] { source[0], source[1], value };
            var sourceLength = source.Length;
            var result = new T[sourceLength + 1];
            Array.Copy(source, result, sourceLength);
            result[sourceLength] = value;
            return result;
        }

        public static Type[] GetParamExprTypes(IList<ParameterExpression> paramExprs)
        {
            if (paramExprs == null || paramExprs.Count == 0)
                return Empty<Type>();

            if (paramExprs.Count == 1)
                return new[] { paramExprs[0].IsByRef ? paramExprs[0].GetResultType().MakeByRefType() : paramExprs[0].GetResultType() };

            var paramTypes = new Type[paramExprs.Count];
            for (var i = 0; i < paramTypes.Length; i++)
                paramTypes[i] = paramExprs[i].IsByRef ? paramExprs[i].GetResultType().MakeByRefType() : paramExprs[i].GetResultType();

            return paramTypes;
        }

        public static Type[] GetParamExprTypes(IList<object> paramExprs)
        {
            if (paramExprs == null || paramExprs.Count == 0)
                return Empty<Type>();

            if (paramExprs.Count == 1)
                return new[] { paramExprs[0].GetResultType() };

            var paramTypes = new Type[paramExprs.Count];
            for (var i = 0; i < paramTypes.Length; i++)
                paramTypes[i] = paramExprs[i].GetResultType();
            return paramTypes;
        }

        public static Type GetFuncOrActionType(Type[] paramTypes, Type returnType)
        {
            if (returnType == typeof(void))
            {
                switch (paramTypes.Length)
                {
                    case 0: return typeof(Action);
                    case 1: return typeof(Action<>).MakeGenericType(paramTypes);
                    case 2: return typeof(Action<,>).MakeGenericType(paramTypes);
                    case 3: return typeof(Action<,,>).MakeGenericType(paramTypes);
                    case 4: return typeof(Action<,,,>).MakeGenericType(paramTypes);
                    case 5: return typeof(Action<,,,,>).MakeGenericType(paramTypes);
                    case 6: return typeof(Action<,,,,,>).MakeGenericType(paramTypes);
                    case 7: return typeof(Action<,,,,,,>).MakeGenericType(paramTypes);
                    default:
                        throw new NotSupportedException(
                            string.Format("Action with so many ({0}) parameters is not supported!", paramTypes.Length));
                }
            }

            paramTypes = paramTypes.WithLast(returnType);
            switch (paramTypes.Length)
            {
                case 1: return typeof(Func<>).MakeGenericType(paramTypes);
                case 2: return typeof(Func<,>).MakeGenericType(paramTypes);
                case 3: return typeof(Func<,,>).MakeGenericType(paramTypes);
                case 4: return typeof(Func<,,,>).MakeGenericType(paramTypes);
                case 5: return typeof(Func<,,,,>).MakeGenericType(paramTypes);
                case 6: return typeof(Func<,,,,,>).MakeGenericType(paramTypes);
                case 7: return typeof(Func<,,,,,,>).MakeGenericType(paramTypes);
                case 8: return typeof(Func<,,,,,,,>).MakeGenericType(paramTypes);
                default:
                    throw new NotSupportedException(
                        string.Format("Func with so many ({0}) parameters is not supported!", paramTypes.Length));
            }
        }

        public static int GetFirstIndex<T>(this IList<T> source, object item)
        {
            if (source == null || source.Count == 0)
                return -1;
            var count = source.Count;
            if (count == 1)
                return ReferenceEquals(source[0], item) ? 0 : -1;
            for (var i = 0; i < count; ++i)
                if (ReferenceEquals(source[i], item))
                    return i;
            return -1;
        }

        public static int GetFirstIndex<T>(this T[] source, Func<T, bool> predicate)
        {
            if (source == null || source.Length == 0)
                return -1;
            if (source.Length == 1)
                return predicate(source[0]) ? 0 : -1;
            for (var i = 0; i < source.Length; ++i)
                if (predicate(source[i]))
                    return i;
            return -1;
        }

        public static T GetFirst<T>(this IEnumerable<T> source)
        {
            var arr = source as T[];
            return arr == null
                ? source.FirstOrDefault()
                : arr.Length != 0 ? arr[0] : default(T);
        }

        public static T GetFirst<T>(this IEnumerable<T> source, Func<T, bool> predicate)
        {
            var arr = source as T[];
            if (arr == null)
                return source.FirstOrDefault(predicate);

            var index = arr.GetFirstIndex(predicate);
            return index == -1 ? default(T) : arr[index];
        }

        public static R[] Project<T, R>(this T[] source, Func<T, R> project)
        {
            if (source == null || source.Length == 0)
                return Empty<R>();

            if (source.Length == 1)
                return new[] { project(source[0]) };

            var result = new R[source.Length];
            for (var i = 0; i < result.Length; ++i)
                result[i] = project(source[i]);
            return result;
        }
    }

#pragma warning disable CS1591 // Missing XML comment for publicly visible type or member
    /// <summary>Facade for constructing expression info.</summary>
    public abstract class ExpressionInfo
    {
        /// <summary>Expression node type.</summary>
        public abstract ExpressionType NodeType { get; }

        /// <summary>All expressions should have a Type.</summary>
        public abstract Type Type { get; }

        /// <summary>Converts back to respective expression so you may Compile it by usual means.</summary>
        public abstract Expression ToExpression();

        /// <summary>Converts to Expression and outputs its as string</summary>
        public override string ToString() => ToExpression().ToString();

        /// <summary>Analog of Expression.Parameter</summary>
        /// <remarks>For now it is return just an `Expression.Parameter`</remarks>
        public static ParameterExpressionInfo Parameter(Type type, string name = null) =>
            new ParameterExpressionInfo(type, name);

        /// <summary>Analog of Expression.Constant</summary>
        public static ConstantExpressionInfo Constant(object value, Type type = null) =>
            value == null && type == null ? _nullExprInfo
                : new ConstantExpressionInfo(value, type ?? value.GetType());

        private static readonly ConstantExpressionInfo
            _nullExprInfo = new ConstantExpressionInfo(null, typeof(object));

        /// <summary>Analog of Expression.New</summary>
        public static NewExpressionInfo New(ConstructorInfo ctor) =>
            new NewExpressionInfo(ctor, Tools.Empty<object>());

        /// <summary>Analog of Expression.New</summary>
        public static NewExpressionInfo New(ConstructorInfo ctor, params object[] arguments) =>
            new NewExpressionInfo(ctor, arguments);

        /// <summary>Analog of Expression.New</summary>
        public static NewExpressionInfo New(ConstructorInfo ctor, params ExpressionInfo[] arguments) =>
            new NewExpressionInfo(ctor, arguments);

        /// <summary>Static method call</summary>
        public static MethodCallExpressionInfo Call(MethodInfo method, params object[] arguments) =>
            new MethodCallExpressionInfo(null, method, arguments);

        /// <summary>Static method call</summary>
        public static MethodCallExpressionInfo Call(MethodInfo method, params ExpressionInfo[] arguments) =>
            new MethodCallExpressionInfo(null, method, arguments);

        /// <summary>Instance method call</summary>
        public static MethodCallExpressionInfo Call(
            ExpressionInfo instance, MethodInfo method, params object[] arguments) =>
            new MethodCallExpressionInfo(instance, method, arguments);

        /// <summary>Instance method call</summary>
        public static MethodCallExpressionInfo Call(
            ExpressionInfo instance, MethodInfo method, params ExpressionInfo[] arguments) =>
            new MethodCallExpressionInfo(instance, method, arguments);

        /// <summary>Static property</summary>
        public static PropertyExpressionInfo Property(PropertyInfo property) =>
            new PropertyExpressionInfo(null, property);

        /// <summary>Instance property</summary>
        public static PropertyExpressionInfo Property(ExpressionInfo instance, PropertyInfo property) =>
            new PropertyExpressionInfo(instance, property);

        /// <summary>Instance property</summary>
        public static PropertyExpressionInfo Property(object instance, PropertyInfo property) =>
            new PropertyExpressionInfo(instance, property);

        /// <summary>Static field</summary>
        public static FieldExpressionInfo Field(FieldInfo field) =>
            new FieldExpressionInfo(null, field);

        /// <summary>Instance field</summary>
        public static FieldExpressionInfo Field(ExpressionInfo instance, FieldInfo field) =>
            new FieldExpressionInfo(instance, field);

        /// <summary>Analog of Expression.Lambda</summary>
        public static LambdaExpressionInfo Lambda(ExpressionInfo body) =>
            new LambdaExpressionInfo(null, body, Tools.Empty<object>());

        /// <summary>Analog of Expression.Lambda</summary>
        public static LambdaExpressionInfo Lambda(ExpressionInfo body,
            params ParameterExpression[] parameters) =>
            new LambdaExpressionInfo(null, body, parameters);

        /// <summary>Analog of Expression.Lambda</summary>
        public static LambdaExpressionInfo Lambda(object body, params object[] parameters) =>
            new LambdaExpressionInfo(null, body, parameters);

        /// <summary>Analog of Expression.Lambda with lambda type specified</summary>
        public static LambdaExpressionInfo Lambda(Type delegateType, object body, params object[] parameters) =>
            new LambdaExpressionInfo(delegateType, body, parameters);

        /// <summary>Analog of Expression.Convert</summary>
        public static UnaryExpressionInfo Convert(ExpressionInfo operand, Type targetType) =>
            new UnaryExpressionInfo(ExpressionType.Convert, operand, targetType);

        /// <summary>Analog of Expression.Lambda</summary>
        public static ExpressionInfo<TDelegate> Lambda<TDelegate>(ExpressionInfo body) =>
            new ExpressionInfo<TDelegate>(body, Tools.Empty<ParameterExpression>());

        /// <summary>Analog of Expression.Lambda</summary>
        public static ExpressionInfo<TDelegate> Lambda<TDelegate>(ExpressionInfo body,
            params ParameterExpression[] parameters) =>
            new ExpressionInfo<TDelegate>(body, parameters);

        /// <summary>Analog of Expression.Lambda</summary>
        public static ExpressionInfo<TDelegate> Lambda<TDelegate>(ExpressionInfo body,
            params ParameterExpressionInfo[] parameters) =>
            new ExpressionInfo<TDelegate>(body, parameters);

        /// <summary>Analog of Expression.ArrayIndex</summary>
        public static BinaryExpressionInfo ArrayIndex(ExpressionInfo array, ExpressionInfo index) =>
            new ArrayIndexExpressionInfo(array, index, array.Type.GetElementType());

        /// <summary>Analog of Expression.ArrayIndex</summary>
        public static BinaryExpressionInfo ArrayIndex(object array, object index) =>
            new ArrayIndexExpressionInfo(array, index, array.GetResultType().GetElementType());

        /// <summary>Expression.Bind used in Expression.MemberInit</summary>
        public static MemberAssignmentInfo Bind(MemberInfo member, ExpressionInfo expression) =>
            new MemberAssignmentInfo(member, expression);

        /// <summary>Analog of Expression.MemberInit</summary>
        public static MemberInitExpressionInfo MemberInit(NewExpressionInfo newExpr,
            params MemberAssignmentInfo[] bindings) =>
            new MemberInitExpressionInfo(newExpr, bindings);

        /// <summary>Enables member assignment on existing instance expression.</summary>
        public static ExpressionInfo MemberInit(ExpressionInfo instanceExpr,
            params MemberAssignmentInfo[] assignments) =>
            new MemberInitExpressionInfo(instanceExpr, assignments);

        /// <summary>Constructs an array given the array type and item initializer expressions.</summary>
        public static NewArrayExpressionInfo NewArrayInit(Type type, params object[] initializers) =>
            new NewArrayExpressionInfo(type, initializers);

        /// <summary>Constructs an array given the array type and item initializer expressions.</summary>
        public static NewArrayExpressionInfo NewArrayInit(Type type, params ExpressionInfo[] initializers) =>
            new NewArrayExpressionInfo(type, initializers);

        /// <summary>Constructs assignment expression.</summary>
        public static ExpressionInfo Assign(ExpressionInfo left, ExpressionInfo right) =>
            new AssignBinaryExpressionInfo(left, right, left.Type);

        /// <summary>Constructs assignment expression from possibly mixed types of left and right.</summary>
        public static ExpressionInfo Assign(object left, object right) =>
            new AssignBinaryExpressionInfo(left, right, left.GetResultType());

        /// <summary>Invoke</summary>
        public static ExpressionInfo Invoke(ExpressionInfo lambda, params object[] args) =>
            new InvocationExpressionInfo(lambda, args, lambda.Type);

        /// <summary>Binary add</summary>
        public static ExpressionInfo Add(ExpressionInfo left, ExpressionInfo right) =>
            new ArithmeticBinaryExpressionInfo(ExpressionType.Add, left, right, left.Type);

        /// <summary>Binary substract</summary>
        public static ExpressionInfo Substract(ExpressionInfo left, ExpressionInfo right) =>
            new ArithmeticBinaryExpressionInfo(ExpressionType.Subtract, left, right, left.Type);

        public static ExpressionInfo Multiply(ExpressionInfo left, ExpressionInfo right) =>
            new ArithmeticBinaryExpressionInfo(ExpressionType.Multiply, left, right, left.Type);

        public static ExpressionInfo Divide(ExpressionInfo left, ExpressionInfo right) =>
            new ArithmeticBinaryExpressionInfo(ExpressionType.Divide, left, right, left.Type);

        public static BlockExpressionInfo Block(params object[] expressions) =>
            new BlockExpressionInfo(expressions[expressions.Length - 1].GetResultType(),
                Tools.Empty<ParameterExpressionInfo>(), expressions);

        public static TryExpressionInfo TryCatch(object body, params CatchBlockInfo[] handlers) =>
            new TryExpressionInfo(body, null, handlers);

        public static TryExpressionInfo TryCatchFinally(object body, ExpressionInfo @finally, params CatchBlockInfo[] handlers) =>
            new TryExpressionInfo(body, @finally, handlers);

        public static TryExpressionInfo TryFinally(object body, ExpressionInfo @finally) =>
            new TryExpressionInfo(body, @finally, null);

        public static CatchBlockInfo Catch(ParameterExpressionInfo variable, ExpressionInfo body) =>
            new CatchBlockInfo(variable, body, null, variable.Type);

        public static CatchBlockInfo Catch(Type test, ExpressionInfo body) =>
            new CatchBlockInfo(null, body, null, test);

        public static UnaryExpressionInfo Throw(ExpressionInfo value) =>
            new UnaryExpressionInfo(ExpressionType.Throw, value, typeof(void));
    }

    public class UnaryExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType { get; }
        public override Type Type { get; }

        public readonly ExpressionInfo Operand;

        public override Expression ToExpression()
        {
            if (NodeType == ExpressionType.Convert)
                return Expression.Convert(Operand.ToExpression(), Type);
            throw new NotSupportedException("Cannot convert ExpressionInfo to Expression of type " + NodeType);
        }

        public UnaryExpressionInfo(ExpressionType nodeType, ExpressionInfo operand, Type type)
        {
            NodeType = nodeType;
            Operand = operand;
            Type = type;
        }
    }

    public abstract class BinaryExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType { get; }
        public override Type Type { get; }

        public readonly object Left, Right;

        protected BinaryExpressionInfo(ExpressionType nodeType, object left, object right, Type type)
        {
            NodeType = nodeType;
            Type = type;
            Left = left;
            Right = right;
        }
    }

    public class ArithmeticBinaryExpressionInfo : BinaryExpressionInfo
    {
        public ArithmeticBinaryExpressionInfo(ExpressionType nodeType, object left, object right, Type type)
            : base(nodeType, left, right, type) { }

        public override Expression ToExpression()
        {
            if (NodeType == ExpressionType.Add)
                return Expression.Add(Left.ToExpression(), Right.ToExpression());
            if (NodeType == ExpressionType.Subtract)
                return Expression.Subtract(Left.ToExpression(), Right.ToExpression());
            if (NodeType == ExpressionType.Multiply)
                return Expression.Multiply(Left.ToExpression(), Right.ToExpression());
            if (NodeType == ExpressionType.Divide)
                return Expression.Divide(Left.ToExpression(), Right.ToExpression());
            throw new NotSupportedException($"Not valid {NodeType} for arithmetic binary expression.");
        }
    }

    public class ArrayIndexExpressionInfo : BinaryExpressionInfo
    {
        public ArrayIndexExpressionInfo(object left, object right, Type type)
            : base(ExpressionType.ArrayIndex, left, right, type) { }

        public override Expression ToExpression() =>
            Expression.ArrayIndex(Left.ToExpression(), Right.ToExpression());
    }

    public class AssignBinaryExpressionInfo : BinaryExpressionInfo
    {
        public AssignBinaryExpressionInfo(object left, object right, Type type)
            : base(ExpressionType.Assign, left, right, type) { }

        public override Expression ToExpression() =>
            Expression.Assign(Left.ToExpression(), Right.ToExpression());
    }

    public class MemberInitExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.MemberInit;
        public override Type Type => ExpressionInfo.Type;

        public NewExpressionInfo NewExpressionInfo => ExpressionInfo as NewExpressionInfo;

        public readonly ExpressionInfo ExpressionInfo;
        public readonly MemberAssignmentInfo[] Bindings;

        public override Expression ToExpression() =>
            Expression.MemberInit(NewExpressionInfo.ToNewExpression(),
                Bindings.Project(b => b.ToMemberAssignment()));

        public MemberInitExpressionInfo(NewExpressionInfo newExpressionInfo, MemberAssignmentInfo[] bindings)
            : this((ExpressionInfo)newExpressionInfo, bindings) { }

        public MemberInitExpressionInfo(ExpressionInfo expressionInfo, MemberAssignmentInfo[] bindings)
        {
            ExpressionInfo = expressionInfo;
            Bindings = bindings ?? Tools.Empty<MemberAssignmentInfo>();
        }
    }

    public class ParameterExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Parameter;
        public override Type Type { get; }

        public readonly string Name;

        public override Expression ToExpression() => ParamExpr;

        public ParameterExpression ParamExpr =>
            _parameter ?? (_parameter = Expression.Parameter(Type, Name));

        public static implicit operator ParameterExpression(ParameterExpressionInfo info) => info.ParamExpr;

        public ParameterExpressionInfo(Type type, string name)
        {
            Type = type;
            Name = name;
        }

        public ParameterExpressionInfo(ParameterExpression paramExpr)
            : this(paramExpr.Type, paramExpr.Name)
        {
            _parameter = paramExpr;
        }

        private ParameterExpression _parameter;
    }

    public class ConstantExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Constant;
        public override Type Type { get; }

        public readonly object Value;

        public override Expression ToExpression() => Expression.Constant(Value, Type);

        public ConstantExpressionInfo(object value, Type type)
        {
            Value = value;
            Type = type;
        }
    }

    public abstract class ArgumentsExpressionInfo : ExpressionInfo
    {
        public readonly object[] Arguments;

        protected Expression[] ArgumentsToExpressions() => Arguments.Project(Tools.ToExpression);

        protected ArgumentsExpressionInfo(object[] arguments)
        {
            Arguments = arguments ?? Tools.Empty<object>();
        }
    }

    public class NewExpressionInfo : ArgumentsExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.New;
        public override Type Type => Constructor.DeclaringType;

        public readonly ConstructorInfo Constructor;

        public override Expression ToExpression() => ToNewExpression();

        public NewExpression ToNewExpression() => Expression.New(Constructor, ArgumentsToExpressions());

        public NewExpressionInfo(ConstructorInfo constructor, params object[] arguments) : base(arguments)
        {
            Constructor = constructor;
        }
    }

    public class NewArrayExpressionInfo : ArgumentsExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.NewArrayInit;
        public override Type Type { get; }

        public override Expression ToExpression() =>
            Expression.NewArrayInit(_elementType, ArgumentsToExpressions());

        public NewArrayExpressionInfo(Type elementType, object[] elements) : base(elements)
        {
            Type = elementType.MakeArrayType();
            _elementType = elementType;
        }

        private readonly Type _elementType;
    }

    public class MethodCallExpressionInfo : ArgumentsExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Call;
        public override Type Type => Method.ReturnType;

        public readonly MethodInfo Method;
        public readonly ExpressionInfo Object;

        public override Expression ToExpression() =>
            Expression.Call(Object?.ToExpression(), Method, ArgumentsToExpressions());

        public MethodCallExpressionInfo(ExpressionInfo @object, MethodInfo method, params object[] arguments)
            : base(arguments)
        {
            Object = @object;
            Method = method;
        }
    }

    public abstract class MemberExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.MemberAccess;
        public readonly MemberInfo Member;

        public readonly object Expression;

        protected MemberExpressionInfo(object expression, MemberInfo member)
        {
            Expression = expression;
            Member = member;
        }
    }

    public class PropertyExpressionInfo : MemberExpressionInfo
    {
        public override Type Type => PropertyInfo.PropertyType;
        public PropertyInfo PropertyInfo => (PropertyInfo)Member;

        public override Expression ToExpression() =>
            System.Linq.Expressions.Expression.Property(Expression.ToExpression(), PropertyInfo);

        public PropertyExpressionInfo(object instance, PropertyInfo property)
            : base(instance, property) { }
    }

    public class FieldExpressionInfo : MemberExpressionInfo
    {
        public override Type Type => FieldInfo.FieldType;
        public FieldInfo FieldInfo => (FieldInfo)Member;

        public override Expression ToExpression() =>
            System.Linq.Expressions.Expression.Field(Expression.ToExpression(), FieldInfo);

        public FieldExpressionInfo(ExpressionInfo instance, FieldInfo field)
            : base(instance, field) { }
    }

    public struct MemberAssignmentInfo
    {
        public MemberInfo Member;
        public ExpressionInfo Expression;

        public MemberBinding ToMemberAssignment() =>
            System.Linq.Expressions.Expression.Bind(Member, Expression.ToExpression());

        public MemberAssignmentInfo(MemberInfo member, ExpressionInfo expression)
        {
            Member = member;
            Expression = expression;
        }
    }

    public class InvocationExpressionInfo : ArgumentsExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Invoke;
        public override Type Type { get; }

        public readonly ExpressionInfo ExprToInvoke;

        public override Expression ToExpression() =>
            Expression.Invoke(ExprToInvoke.ToExpression(), ArgumentsToExpressions());

        public InvocationExpressionInfo(ExpressionInfo exprToInvoke, object[] arguments, Type type) : base(arguments)
        {
            ExprToInvoke = exprToInvoke;
            Type = type;
        }
    }

    public class BlockExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Block;

        public override Type Type { get; }

        public readonly ParameterExpressionInfo[] Variables;
        public readonly object[] Expressions;
        public readonly object Result;

        public override Expression ToExpression() =>
            Expression.Block(Expressions.Project(Tools.ToExpression));

        public BlockExpressionInfo(Type type, ParameterExpressionInfo[] variables, object[] expressions)
        {
            Variables = variables;
            Expressions = expressions;
            Result = expressions[expressions.Length - 1];
            Type = type;
        }
    }

    public class TryExpressionInfo : ExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Try;
        public override Type Type { get; }

        public readonly object Body;
        public readonly CatchBlockInfo[] Handlers;
        public readonly ExpressionInfo Finally;

        public override Expression ToExpression() =>
            Finally == null ? Expression.TryCatch(Body.ToExpression(), ToCatchBlocks(Handlers)) :
            Handlers == null ? Expression.TryFinally(Body.ToExpression(), Finally.ToExpression()) :
            Expression.TryCatchFinally(Body.ToExpression(), Finally.ToExpression(), ToCatchBlocks(Handlers));

        private static CatchBlock[] ToCatchBlocks(CatchBlockInfo[] hs)
        {
            if (hs == null)
                return Tools.Empty<CatchBlock>();
            var catchBlocks = new CatchBlock[hs.Length];
            for (var i = 0; i < hs.Length; ++i)
                catchBlocks[i] = hs[i].ToCatchBlock();
            return catchBlocks;
        }

        public TryExpressionInfo(object body, ExpressionInfo @finally, CatchBlockInfo[] handlers)
        {
            Type = body.GetResultType();
            Body = body;
            Handlers = handlers;
            Finally = @finally;
        }
    }

    public sealed class CatchBlockInfo
    {
        public readonly ParameterExpressionInfo Variable;
        public readonly ExpressionInfo Body;
        public readonly ExpressionInfo Filter;
        public readonly Type Test;

        public CatchBlockInfo(ParameterExpressionInfo variable, ExpressionInfo body, ExpressionInfo filter, Type test)
        {
            Variable = variable;
            Body = body;
            Filter = filter;
            Test = test;
        }

        public CatchBlock ToCatchBlock() => Expression.Catch(Variable.ParamExpr, Body.ToExpression());
    }

    public class LambdaExpressionInfo : ArgumentsExpressionInfo
    {
        public override ExpressionType NodeType => ExpressionType.Lambda;
        public override Type Type { get; }

        public readonly object Body;
        public object[] Parameters => Arguments;

        public override Expression ToExpression() => ToLambdaExpression();

        public LambdaExpression ToLambdaExpression() =>
            Expression.Lambda(Body.ToExpression(),
                Parameters.Project(p => (ParameterExpression)p.ToExpression()));

        public LambdaExpressionInfo(Type delegateType, object body, object[] parameters) : base(parameters)
        {
            Body = body;
            var bodyType = body.GetResultType();
            Type = delegateType != null && delegateType != typeof(Delegate)
                ? delegateType
                : Tools.GetFuncOrActionType(Tools.GetParamExprTypes(parameters), bodyType);
        }
    }

    public sealed class ExpressionInfo<TDelegate> : LambdaExpressionInfo
    {
        public Type DelegateType => Type;

        public override Expression ToExpression() => ToLambdaExpression();

        public new Expression<TDelegate> ToLambdaExpression() =>
            Expression.Lambda<TDelegate>(Body.ToExpression(),
                Parameters.Project(p => (ParameterExpression)p.ToExpression()));

        public ExpressionInfo(ExpressionInfo body, object[] parameters)
            : base(typeof(TDelegate), body, parameters) { }
    }
#pragma warning restore CS1591 // Missing XML comment for publicly visible type or member
}

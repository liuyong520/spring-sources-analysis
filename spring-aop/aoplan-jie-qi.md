# 拦截器
拦截器可以说是AOP实现的精髓了，基本AOP的所有的增强方法都是通过拦截器去实现的。

# MethodInterceptor
通过方法拦截器，把我们的advice通知 增强 应用到对应的方法上从而实现AOP的功能

那么具体SpringAop是如何干的呢？
```java
public DefaultAdvisorAdapterRegistry() {
    //注册了三个通知适配者     
    registerAdvisorAdapter(new MethodBeforeAdviceAdapter());
    registerAdvisorAdapter(new AfterReturningAdviceAdapter());
    registerAdvisorAdapter(new ThrowsAdviceAdapter());
}
```
```java
public MethodInterceptor[] getInterceptors(Advisor advisor) throws UnknownAdviceTypeException {
    List<MethodInterceptor> interceptors = new ArrayList<MethodInterceptor>(3);
    // 获取通知
    Advice advice = advisor.getAdvice();
    // 如果通知属于方法拦截器，则直接添加无须适配
    if (advice instanceof MethodInterceptor) {
        interceptors.add((MethodInterceptor) advice);
    }
    // 否则就遍历当前的已有的适配器，
    for (AdvisorAdapter adapter : this.adapters) {
        //看看哪个适配器支持现有的通知，如果至此就把对应的advice封装成MethodInterceptor的实现类，添加到拦截器链中
        if (adapter.supportsAdvice(advice)) {
            interceptors.add(adapter.getInterceptor(advisor));
        }
    }
    if (interceptors.isEmpty()) {
        throw new UnknownAdviceTypeException(advisor.getAdvice());
    }
    //返回拦截器数组
    return interceptors.toArray(new MethodInterceptor[interceptors.size()]);
}
```
拦截器执行的过程中，最终会调用invoke方法

# MethodBeforeAdviceInterceptor
```java
public class MethodBeforeAdviceInterceptor implements MethodInterceptor, Serializable {

    private MethodBeforeAdvice advice;


    /**
     * Create a new MethodBeforeAdviceInterceptor for the given advice.
     * @param advice the MethodBeforeAdvice to wrap
     */
    public MethodBeforeAdviceInterceptor(MethodBeforeAdvice advice) {
        Assert.notNull(advice, "Advice must not be null");
        this.advice = advice;
    }

    @Override
    public Object invoke(MethodInvocation mi) throws Throwable {
        this.advice.before(mi.getMethod(), mi.getArguments(), mi.getThis() );
        return mi.proceed();
    }

}
```
其实很简单，在方法执行之前，会调用增强方法。
# AfterReturningAdviceInterceptor
```
public class AfterReturningAdviceInterceptor implements MethodInterceptor, AfterAdvice, Serializable {

    private final AfterReturningAdvice advice;


    /**
     * Create a new AfterReturningAdviceInterceptor for the given advice.
     * @param advice the AfterReturningAdvice to wrap
     */
    public AfterReturningAdviceInterceptor(AfterReturningAdvice advice) {
        Assert.notNull(advice, "Advice must not be null");
        this.advice = advice;
    }

    @Override
    public Object invoke(MethodInvocation mi) throws Throwable {
        Object retVal = mi.proceed();
        this.advice.afterReturning(retVal, mi.getMethod(), mi.getArguments(), mi.getThis());
        return retVal;
    }

}
```
# ThrowsAdviceInterceptor

```java
public class ThrowsAdviceInterceptor implements MethodInterceptor, AfterAdvice {

    private static final String AFTER_THROWING = "afterThrowing";

    private static final Log logger = LogFactory.getLog(ThrowsAdviceInterceptor.class);


    private final Object throwsAdvice;

    /** Methods on throws advice, keyed by exception class */
    private final Map<Class<?>, Method> exceptionHandlerMap = new HashMap<Class<?>, Method>();


    /**
     * Create a new ThrowsAdviceInterceptor for the given ThrowsAdvice.
     * @param throwsAdvice the advice object that defines the exception
     * handler methods (usually a {@link org.springframework.aop.ThrowsAdvice}
     * implementation)
     */
    public ThrowsAdviceInterceptor(Object throwsAdvice) {
        Assert.notNull(throwsAdvice, "Advice must not be null");
        this.throwsAdvice = throwsAdvice;

        Method[] methods = throwsAdvice.getClass().getMethods();
        for (Method method : methods) {
            if (method.getName().equals(AFTER_THROWING) &&
                    (method.getParameterTypes().length == 1 || method.getParameterTypes().length == 4) &&
                    Throwable.class.isAssignableFrom(method.getParameterTypes()[method.getParameterTypes().length - 1])
                ) {
                // Have an exception handler
                this.exceptionHandlerMap.put(method.getParameterTypes()[method.getParameterTypes().length - 1], method);
                if (logger.isDebugEnabled()) {
                    logger.debug("Found exception handler method: " + method);
                }
            }
        }

        if (this.exceptionHandlerMap.isEmpty()) {
            throw new IllegalArgumentException(
                    "At least one handler method must be found in class [" + throwsAdvice.getClass() + "]");
        }
    }

    public int getHandlerMethodCount() {
        return this.exceptionHandlerMap.size();
    }

    /**
     * Determine the exception handle method. Can return null if not found.
     * @param exception the exception thrown
     * @return a handler for the given exception type
     */
    private Method getExceptionHandler(Throwable exception) {
        Class<?> exceptionClass = exception.getClass();
        if (logger.isTraceEnabled()) {
            logger.trace("Trying to find handler for exception of type [" + exceptionClass.getName() + "]");
        }
        Method handler = this.exceptionHandlerMap.get(exceptionClass);
        while (handler == null && exceptionClass != Throwable.class) {
            exceptionClass = exceptionClass.getSuperclass();
            handler = this.exceptionHandlerMap.get(exceptionClass);
        }
        if (handler != null && logger.isDebugEnabled()) {
            logger.debug("Found handler for exception of type [" + exceptionClass.getName() + "]: " + handler);
        }
        return handler;
    }

    @Override
    public Object invoke(MethodInvocation mi) throws Throwable {
        try {
            return mi.proceed();
        }
        catch (Throwable ex) {
            Method handlerMethod = getExceptionHandler(ex);
            if (handlerMethod != null) {
                invokeHandlerMethod(mi, ex, handlerMethod);
            }
            throw ex;
        }
    }

    private void invokeHandlerMethod(MethodInvocation mi, Throwable ex, Method method) throws Throwable {
        Object[] handlerArgs;
        if (method.getParameterTypes().length == 1) {
            handlerArgs = new Object[] { ex };
        }
        else {
            handlerArgs = new Object[] {mi.getMethod(), mi.getArguments(), mi.getThis(), ex};
        }
        try {
            method.invoke(this.throwsAdvice, handlerArgs);
        }
        catch (InvocationTargetException targetEx) {
            throw targetEx.getTargetException();
        }
    }

}
```
AOP中所有的增强都是基于这三种通知来实现的。

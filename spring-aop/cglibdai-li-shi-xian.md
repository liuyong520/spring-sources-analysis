# CGLIB代理
## getProxy接口实现
### CGLIB代理实现
```java
public Object getProxy(ClassLoader classLoader) {
        try {
            // 获取代理类class对象
            Class<?> rootClass = this.advised.getTargetClass();
            Assert.state(rootClass != null, "Target class must be available for creating a CGLIB proxy");

            Class<?> proxySuperClass = rootClass;
            //判断当前class对象是不是已经是代理对象，如果是就需要获取到class的父类，同时保存当前类的所有接口到本地
            if (ClassUtils.isCglibProxyClass(rootClass)) {
                proxySuperClass = rootClass.getSuperclass();
                Class<?>[] additionalInterfaces = rootClass.getInterfaces();
                for (Class<?> additionalInterface : additionalInterfaces) {
                    this.advised.addInterface(additionalInterface);
                }
            }

            // Validate the class, writing log messages as necessary.
            // 检查父类中的方法的修饰符，并进行对应日志的输出，没什么实际鸟用，
            validateClassIfNecessary(proxySuperClass, classLoader);

            // Configure CGLIB Enhancer...
            // 创建并配置CGLIB的Enhancer对象，这个对象是CGLIB的主要操作类
            Enhancer enhancer = createEnhancer();
            if (classLoader != null) {
                enhancer.setClassLoader(classLoader);
                if (classLoader instanceof SmartClassLoader &&
                        ((SmartClassLoader) classLoader).isClassReloadable(proxySuperClass)) {
                    enhancer.setUseCache(false);
                }
            }
            //设置Enhancer对象，包括设置代理接口，回调方法，
            enhancer.setSuperclass(proxySuperClass);
            enhancer.setInterfaces(AopProxyUtils.completeProxiedInterfaces(this.advised));
            enhancer.setNamingPolicy(SpringNamingPolicy.INSTANCE);
            enhancer.setStrategy(new ClassLoaderAwareUndeclaredThrowableStrategy(classLoader));

            Callback[] callbacks = getCallbacks(rootClass);
            Class<?>[] types = new Class<?>[callbacks.length];
            for (int x = 0; x < types.length; x++) {
                types[x] = callbacks[x].getClass();
            }
            // fixedInterceptorMap only populated at this point, after getCallbacks call above
            // 这里就是设置拦截器链
            enhancer.setCallbackFilter(new ProxyCallbackFilter(
                    this.advised.getConfigurationOnlyCopy(), this.fixedInterceptorMap, this.fixedInterceptorOffset));
            enhancer.setCallbackTypes(types);
            // 创建代理对象实例，
            return createProxyClassAndInstance(enhancer, callbacks);
        }
        ···省略掉异常catch
    }
```

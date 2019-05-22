# bean 实例化
 查看processBeanDefinition
```java
protected void processBeanDefinition(Element ele, BeanDefinitionParserDelegate delegate) {
    BeanDefinitionHolder bdHolder = delegate.parseBeanDefinitionElement(ele);
    if (bdHolder != null) {
        // 装饰bean装饰哪些不是默认namespace的那些bean
        // 1.去META-INF/spring.handlers下加载namespaceHandler
        // 2.根据namespace的的值拿到对应的namespaceHandler
        // 3.获取namespaceHandler的实例，然后调用namespaceHandler的decorate的方法
        bdHolder = delegate.decorateBeanDefinitionIfRequired(ele, bdHolder);
        try {
            // Register the final decorated instance.
            BeanDefinitionReaderUtils.registerBeanDefinition(bdHolder, getReaderContext().getRegistry());
        }
        catch (BeanDefinitionStoreException ex) {
            getReaderContext().error("Failed to register bean definition with name '" +
                    bdHolder.getBeanName() + "'", ele, ex);
        }
        // Send registration event.
        getReaderContext().fireComponentRegistered(new BeanComponentDefinition(bdHolder));
    }
}

```
## bean装饰

### delegate.decorateBeanDefinitionIfRequired
这一块和之前讲的自定义标签解析是一样的实现。都是用策略模式实现的，都是通过去找namespaceHandler接口，然后实例化，调用装饰方法，完成个性化的操作。这里就不再贴代码详细介绍了
```java
public BeanDefinitionHolder decorateBeanDefinitionIfRequired(
        Element ele, BeanDefinitionHolder definitionHolder, BeanDefinition containingBd) {

    BeanDefinitionHolder finalDefinition = definitionHolder;

    // Decorate based on custom attributes first.
    // 装饰自定义属性标签
    NamedNodeMap attributes = ele.getAttributes();
    for (int i = 0; i < attributes.getLength(); i++) {
        Node node = attributes.item(i);
        finalDefinition = decorateIfRequired(node, finalDefinition, containingBd);
    }

    // Decorate based on custom nested elements.
    // 装饰自定义节点标签
    NodeList children = ele.getChildNodes();
    for (int i = 0; i < children.getLength(); i++) {
        Node node = children.item(i);
        if (node.getNodeType() == Node.ELEMENT_NODE) {
            finalDefinition = decorateIfRequired(node, finalDefinition, containingBd);
        }
    }
    return finalDefinition;
} 
```
## bean的注册
```java
@Override
    public void registerBeanDefinition(String beanName, BeanDefinition beanDefinition)
            throws BeanDefinitionStoreException {
    //前面介绍过了bean定义的加载
    if (beanDefinition instanceof AbstractBeanDefinition) {
        try {
            //校验
            ((AbstractBeanDefinition) beanDefinition).validate();
        }
        catch (BeanDefinitionValidationException ex) {
            throw new BeanDefinitionStoreException(beanDefinition.getResourceDescription(), beanName,
                    "Validation of bean definition failed", ex);
        }
    }

    BeanDefinition oldBeanDefinition;

    oldBeanDefinition = this.beanDefinitionMap.get(beanName);
    if (oldBeanDefinition != null) {
        if (!isAllowBeanDefinitionOverriding()) {
          // 如果不允许重复定义就抛出异常
          ...
        }
        //如果可以被覆盖就比较一下
        else if (oldBeanDefinition.getRole() < beanDefinition.getRole()) {
            // e.g. was ROLE_APPLICATION, now overriding with ROLE_SUPPORT or ROLE_INFRASTRUCTURE
            if (this.logger.isWarnEnabled()) {
                this.logger.warn("Overriding user-defined bean definition for bean '" + beanName +
                        "' with a framework-generated bean definition: replacing [" +
                        oldBeanDefinition + "] with [" + beanDefinition + "]");
            }
        }
        else if (!beanDefinition.equals(oldBeanDefinition)) {
            if (this.logger.isInfoEnabled()) {
                this.logger.info("Overriding bean definition for bean '" + beanName +
                        "' with a different definition: replacing [" + oldBeanDefinition +
                        "] with [" + beanDefinition + "]");
            }
        }
        else {
            if (this.logger.isDebugEnabled()) {
                this.logger.debug("Overriding bean definition for bean '" + beanName +
                        "' with an equivalent definition: replacing [" + oldBeanDefinition +
                        "] with [" + beanDefinition + "]");
            }
        }
        // 覆盖原来的bean
        this.beanDefinitionMap.put(beanName, beanDefinition);
    }
    else {

        if (hasBeanCreationStarted()) {
            // Cannot modify startup-time collection elements anymore (for stable iteration)
            synchronized (this.beanDefinitionMap) {
                // 添加到beanDefinitionMap里面
                this.beanDefinitionMap.put(beanName, beanDefinition);
                List<String> updatedDefinitions = new ArrayList<String>(this.beanDefinitionNames.size() + 1);
                updatedDefinitions.addAll(this.beanDefinitionNames);
                updatedDefinitions.add(beanName);
                // 把beanNanme添加到list里面
                this.beanDefinitionNames = updatedDefinitions;
                // 如果原来单例bean里面
                if (this.manualSingletonNames.contains(beanName)) {
                    // 移除掉beanName 这里为什么要移除掉？
                    // 留坑
                    Set<String> updatedSingletons = new LinkedHashSet<String>(this.manualSingletonNames);
                    updatedSingletons.remove(beanName);
                    this.manualSingletonNames = updatedSingletons;
                }
            }
        }
        else {
            // Still in startup registration phase
            // 这里表示是启动的时候缓存相关信息
            this.beanDefinitionMap.put(beanName, beanDefinition);
            this.beanDefinitionNames.add(beanName);
            this.manualSingletonNames.remove(beanName);
        }
        this.frozenBeanDefinitionNames = null;
    }

    if (oldBeanDefinition != null || containsSingleton(beanName)) {
        //重置一下
        resetBeanDefinition(beanName);
    }
}
```
bean的实例化
```java
public void preInstantiateSingletons() throws BeansException {
    //获取所有的beanName
    List<String> beanNames = new ArrayList<String>(this.beanDefinitionNames);

    // Trigger initialization of all non-lazy singleton beans...
    for (String beanName : beanNames) {
        // Bean定义公共的抽象类是AbstractBeanDefinition，
        // 普通的Bean在Spring加载Bean定义的时候，
        // 实例化出来的是GenericBeanDefinition，
        // 而Spring上下文包括实例化所有Bean用的AbstractBeanDefinition是RootBeanDefinition，
        // 这时候就使用getMergedLocalBeanDefinition方法做了一次转化，
        // 将非RootBeanDefinition转换为RootBeanDefinition以供后续操作。
        RootBeanDefinition bd = getMergedLocalBeanDefinition(beanName);
        if (!bd.isAbstract() && bd.isSingleton() && !bd.isLazyInit()) {
            //判断是不是FactoryBean
            //FactoryBean是一种特殊的bean。可以通过&beanName的去拿到真实的bean
            //其实就是调用的FactoryBean接口的getObject方法
            if (isFactoryBean(beanName)) {
                final FactoryBean<?> factory = (FactoryBean<?>) getBean(FACTORY_BEAN_PREFIX + beanName);
                boolean isEagerInit;
                if (System.getSecurityManager() != null && factory instanceof SmartFactoryBean) {
                    isEagerInit = AccessController.doPrivileged(new PrivilegedAction<Boolean>() {
                        @Override
                        public Boolean run() {
                            return ((SmartFactoryBean<?>) factory).isEagerInit();
                        }
                    }, getAccessControlContext());
                }
                else {
                    isEagerInit = (factory instanceof SmartFactoryBean &&
                            ((SmartFactoryBean<?>) factory).isEagerInit());
                }
                //如果被标记为EagerInit就需要被立即初始化
                if (isEagerInit) {
                    getBean(beanName);
                }
            }
            else {
                //如果是是单例bean就初始化
                getBean(beanName);
            }
        }
    }
    // 初始化完成后调用post-initialization callback回调方法。
    // Trigger post-initialization callback for all applicable beans...
    for (String beanName : beanNames) {

        Object singletonInstance = getSingleton(beanName);
        //如果继承了SmartInitializingSingleton接口所有单例bean都会执行回调方法afterSingletonsInstantiated
        if (singletonInstance instanceof SmartInitializingSingleton) {
            final SmartInitializingSingleton smartSingleton = (SmartInitializingSingleton) singletonInstance;
            if (System.getSecurityManager() != null) {
                AccessController.doPrivileged(new PrivilegedAction<Object>() {
                    @Override
                    public Object run() {
                        smartSingleton.afterSingletonsInstantiated();
                        return null;
                    }
                }, getAccessControlContext());
            }
            else {
                smartSingleton.afterSingletonsInstantiated();
            }
        }
    }
}
```
实例化操作是在getbean方法里面完成的。
### getbean
```java
@Override
public Object getBean(String name) throws BeansException {
    return doGetBean(name, null, null, false);
}

```
#### doGetBean
这个方法有点长，一些日志代码我就干掉了，保留核心代码
```java
protected <T> T doGetBean(
            final String name, final Class<T> requiredType, final Object[] args, boolean typeCheckOnly)
            throws BeansException {
    // 获取真实的beanName        
    final String beanName = transformedBeanName(name);
    Object bean;

    // Eagerly check singleton cache for manually registered singletons.
    // 获取单例bean的实例
    Object sharedInstance = getSingleton(beanName);
    if (sharedInstance != null && args == null) {
        //
        bean = getObjectForBeanInstance(sharedInstance, name, beanName, null);
    }

    else {
        // Fail if we're already creating this bean instance:
        // We're assumably within a circular reference.
        if (isPrototypeCurrentlyInCreation(beanName)) {
            throw new BeanCurrentlyInCreationException(beanName);
        }

        // Check if bean definition exists in this factory.
        BeanFactory parentBeanFactory = getParentBeanFactory();
        if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
            // Not found -> check parent.
            String nameToLookup = originalBeanName(name);
            if (args != null) {
                // Delegation to parent with explicit args.
                return (T) parentBeanFactory.getBean(nameToLookup, args);
            }
            else {
                // No args -> delegate to standard getBean method.
                return parentBeanFactory.getBean(nameToLookup, requiredType);
            }
        }
        //类型检查
        if (!typeCheckOnly) {
            //把beanName标记为已经创建，其实就是往alreadyCreated set 集合里面写一条数据
            markBeanAsCreated(beanName);
        }

        try {
            //转换类型
            final RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
            checkMergedBeanDefinition(mbd, beanName, args);

            // Guarantee initialization of beans that the current bean depends on.
            // 检查depends-on 依赖
            String[] dependsOn = mbd.getDependsOn();
            if (dependsOn != null) {
                for (String dependsOnBean : dependsOn) {
                    if (isDependent(beanName, dependsOnBean)) {
                      ...
                    }
                    //注册依赖的bean
                    registerDependentBean(dependsOnBean, beanName);
                    //实例化依赖的bean
                    getBean(dependsOnBean);
                }
            }

            // Create bean instance.
            if (mbd.isSingleton()) {
                sharedInstance = getSingleton(beanName, new ObjectFactory<Object>() {
                    @Override
                    public Object getObject() throws BeansException {
                        try {
                            //创建bean的实例
                            return createBean(beanName, mbd, args);
                        }
                        catch (BeansException ex) {
                            destroySingleton(beanName);
                            throw ex;
                        }
                    }
                });
                bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
            }
            //实例化多例bean
            else if (mbd.isPrototype()) {
                // It's a prototype -> create a new instance.
                Object prototypeInstance = null;
                try {
                    beforePrototypeCreation(beanName);
                    prototypeInstance = createBean(beanName, mbd, args);
                }
                finally {
                    afterPrototypeCreation(beanName);
                }
                bean = getObjectForBeanInstance(prototypeInstance, name, beanName, mbd);
            }

            else {
                String scopeName = mbd.getScope();
                final Scope scope = this.scopes.get(scopeName);
                if (scope == null) {
                    throw new IllegalStateException("No Scope registered for scope name '" + scopeName + "'");
                }
                try {
                    Object scopedInstance = scope.get(beanName, new ObjectFactory<Object>() {
                        @Override
                        public Object getObject() throws BeansException {
                            beforePrototypeCreation(beanName);
                            try {
                                return createBean(beanName, mbd, args);
                            }
                            finally {
                                afterPrototypeCreation(beanName);
                            }
                        }
                    });
                    bean = getObjectForBeanInstance(scopedInstance, name, beanName, mbd);
                }
                catch (IllegalStateException ex) {
                   ...
                }
            }
        }
        catch (BeansException ex) {
            cleanupAfterBeanCreationFailure(beanName);
            throw ex;
        }
    }

    // Check if required type matches the type of the actual bean instance.
    if (requiredType != null && bean != null && !requiredType.isAssignableFrom(bean.getClass())) {
        try {
            return getTypeConverter().convertIfNecessary(bean, requiredType);
        }
        catch (TypeMismatchException ex) {
            ...
        }
    }
    return (T) bean;
}
```

```java
protected Object getObjectForBeanInstance(
            Object beanInstance, String name, String beanName, RootBeanDefinition mbd) {

    // Don't let calling code try to dereference the factory if the bean isn't a factory.
    if (BeanFactoryUtils.isFactoryDereference(name) && !(beanInstance instanceof FactoryBean)) {
        throw new BeanIsNotAFactoryException(transformedBeanName(name), beanInstance.getClass());
    }

    // Now we have the bean instance, which may be a normal bean or a FactoryBean.
    // If it's a FactoryBean, we use it to create a bean instance, unless the
    // caller actually wants a reference to the factory.
    if (!(beanInstance instanceof FactoryBean) || BeanFactoryUtils.isFactoryDereference(name)) {
        return beanInstance;
    }

    Object object = null;
    if (mbd == null) {
        object = getCachedObjectForFactoryBean(beanName);
    }
    if (object == null) {
        // Return bean instance from factory.
        FactoryBean<?> factory = (FactoryBean<?>) beanInstance;
        // Caches object obtained from FactoryBean if it is a singleton.
        if (mbd == null && containsBeanDefinition(beanName)) {
            mbd = getMergedLocalBeanDefinition(beanName);
        }
        boolean synthetic = (mbd != null && mbd.isSynthetic());
        object = getObjectFromFactoryBean(factory, beanName, !synthetic);
    }
    return object;
}
```
### createBean
createBean方法是AbstractBeanFactory的子类AbstractAutowireCapableBeanFactory的一个方法，看一下它的方法实现：

```java
protected Object createBean(final String beanName, final RootBeanDefinition mbd, final Object[] args)
        throws BeanCreationException {

    if (logger.isDebugEnabled()) {
        logger.debug("Creating instance of bean '" + beanName + "'");
    }
    // Make sure bean class is actually resolved at this point.
    resolveBeanClass(mbd, beanName);

    // Prepare method overrides.
    try {
        mbd.prepareMethodOverrides();
    }
    catch (BeanDefinitionValidationException ex) {
        throw new BeanDefinitionStoreException(mbd.getResourceDescription(),
                beanName, "Validation of method overrides failed", ex);
    }

    try {
        // Give BeanPostProcessors a chance to return a proxy instead of the target bean instance.
        // 这里是spring提供一些接口扩展，这里会触发所有的BeanPostProcessor前置处理器和后置处理器
        Object bean = resolveBeforeInstantiation(beanName, mbd);
        if (bean != null) {
            return bean;
        }
    }
    catch (Throwable ex) {
        throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                "BeanPostProcessor before instantiation of bean failed", ex);
    }

    Object beanInstance = doCreateBean(beanName, mbd, args);
    if (logger.isDebugEnabled()) {
        logger.debug("Finished creating instance of bean '" + beanName + "'");
    }
    return beanInstance;
}
```
重要的方法是doCreateBean
### doCreateBean
```java
protected Object doCreateBean(final String beanName, final RootBeanDefinition mbd, final Object[] args) {
        // Instantiate the bean.
        BeanWrapper instanceWrapper = null;
        // 如果是单例bean
        if (mbd.isSingleton()) {
            //首先从factoryBean的缓存里面找factoryBean的包装类。
            instanceWrapper = this.factoryBeanInstanceCache.remove(beanName);
        }
        if (instanceWrapper == null) {
            // 如果 factoryBean缓存里面没有对应名字的factoryBean就自行创建。
            instanceWrapper = createBeanInstance(beanName, mbd, args);
        }
        // 从instanceWrapper赋值
        final Object bean = (instanceWrapper != null ? instanceWrapper.getWrappedInstance() : null);
        Class<?> beanType = (instanceWrapper != null ? instanceWrapper.getWrappedClass() : null);

        // Allow post-processors to modify the merged bean definition.
        // 这个也可以说是spring的扩展接口了，只需要继承MergedBeanDefinitionPostProcessor接口，就可以完成对beanDefinition的一些修改。   
        synchronized (mbd.postProcessingLock) {
            if (!mbd.postProcessed) {

                applyMergedBeanDefinitionPostProcessors(mbd, beanType, beanName);
                mbd.postProcessed = true;
            }
        }

        // Eagerly cache singletons to be able to resolve circular references
        // even when triggered by lifecycle interfaces like BeanFactoryAware.
        boolean earlySingletonExposure = (mbd.isSingleton() && this.allowCircularReferences &&
                isSingletonCurrentlyInCreation(beanName));
        if (earlySingletonExposure) {
            if (logger.isDebugEnabled()) {
                logger.debug("Eagerly caching bean '" + beanName +
                        "' to allow for resolving potential circular references");
            }
            addSingletonFactory(beanName, new ObjectFactory<Object>() {
                @Override
                public Object getObject() throws BeansException {
                    return getEarlyBeanReference(beanName, mbd, bean);
                }
            });
        }

        // Initialize the bean instance.
        Object exposedObject = bean;
        try {
            //关键方法
            //
            populateBean(beanName, mbd, instanceWrapper);
            if (exposedObject != null) {
                exposedObject = initializeBean(beanName, exposedObject, mbd);
            }
        }
        catch (Throwable ex) {
            if (ex instanceof BeanCreationException && beanName.equals(((BeanCreationException) ex).getBeanName())) {
                throw (BeanCreationException) ex;
            }
            else {
                throw new BeanCreationException(mbd.getResourceDescription(), beanName, "Initialization of bean failed", ex);
            }
        }

        if (earlySingletonExposure) {
            Object earlySingletonReference = getSingleton(beanName, false);
            if (earlySingletonReference != null) {
                if (exposedObject == bean) {
                    exposedObject = earlySingletonReference;
                }
                else if (!this.allowRawInjectionDespiteWrapping && hasDependentBean(beanName)) {
                    String[] dependentBeans = getDependentBeans(beanName);
                    Set<String> actualDependentBeans = new LinkedHashSet<String>(dependentBeans.length);
                    for (String dependentBean : dependentBeans) {
                        if (!removeSingletonIfCreatedForTypeCheckOnly(dependentBean)) {
                            actualDependentBeans.add(dependentBean);
                        }
                    }
                    if (!actualDependentBeans.isEmpty()) {
                        throw new BeanCurrentlyInCreationException(beanName,
                                "Bean with name '" + beanName + "' has been injected into other beans [" +
                                StringUtils.collectionToCommaDelimitedString(actualDependentBeans) +
                                "] in its raw version as part of a circular reference, but has eventually been " +
                                "wrapped. This means that said other beans do not use the final version of the " +
                                "bean. This is often the result of over-eager type matching - consider using " +
                                "'getBeanNamesOfType' with the 'allowEagerInit' flag turned off, for example.");
                    }
                }
            }
        }

        // Register bean as disposable.
        try {
            registerDisposableBeanIfNecessary(beanName, bean, mbd);
        }
        catch (BeanDefinitionValidationException ex) {
            throw new BeanCreationException(mbd.getResourceDescription(), beanName, "Invalid destruction signature", ex);
        }

        return exposedObject;
    }
```

###
```java
protected BeanWrapper createBeanInstance(String beanName, RootBeanDefinition mbd, Object[] args) {
        // Make sure bean class is actually resolved at this point.
        Class<?> beanClass = resolveBeanClass(mbd, beanName);

        if (beanClass != null && !Modifier.isPublic(beanClass.getModifiers()) && !mbd.isNonPublicAccessAllowed()) {
            throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                    "Bean class isn't public, and non-public access not allowed: " + beanClass.getName());
        }
        // 如果有工厂方法，factoryMethod 就调用factroyMethod创建  
        if (mbd.getFactoryMethodName() != null)  {
            return instantiateUsingFactoryMethod(beanName, mbd, args);
        }

        // Shortcut when re-creating the same bean...
        boolean resolved = false;
        boolean autowireNecessary = false;
        // 这里会检查bean里面的依赖注入
        if (args == null) {
            synchronized (mbd.constructorArgumentLock) {
                //检查是否有构造方法或者工厂方法
                if (mbd.resolvedConstructorOrFactoryMethod != null) {
                    resolved = true;
                    autowireNecessary = mbd.constructorArgumentsResolved;
                }
            }
        }
        //true
        if (resolved) {
            if (autowireNecessary) {
                // 就采用构造方法生成bean
                return autowireConstructor(beanName, mbd, null, null);
            }
            else {
                //否则就是属性方法生成bean
                return instantiateBean(beanName, mbd);
            }
        }

        // Need to determine the constructor...
        Constructor<?>[] ctors = determineConstructorsFromBeanPostProcessors(beanClass, beanName);
        if (ctors != null ||
                mbd.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_CONSTRUCTOR ||
                mbd.hasConstructorArgumentValues() || !ObjectUtils.isEmpty(args))  {
            return autowireConstructor(beanName, mbd, ctors, args);
        }

        // No special handling: simply use no-arg constructor.
        // 这里首先通过无参数的方法创建实例
        // 然后通过cglib代理的方式创建属性实例，用属性的方式注入到bean里面
        return instantiateBean(beanName, mbd);
    }

```
继续：
### autowireConstructor
```java
protected BeanWrapper autowireConstructor(
            String beanName, RootBeanDefinition mbd, Constructor<?>[] ctors, Object[] explicitArgs) {

    return new ConstructorResolver(this).autowireConstructor(beanName, mbd, ctors, explicitArgs);
}
```
autowireConstructor这个方法比较长：
```java
public BeanWrapper autowireConstructor(final String beanName, final RootBeanDefinition mbd,
            Constructor<?>[] chosenCtors, final Object[] explicitArgs) {
    //初始化BeanWrapperImpl        
    BeanWrapperImpl bw = new BeanWrapperImpl();
    //设置对应的类型转换器
    this.beanFactory.initBeanWrapper(bw);

    Constructor<?> constructorToUse = null;
    ArgumentsHolder argsHolderToUse = null;
    Object[] argsToUse = null;

    if (explicitArgs != null) {
        argsToUse = explicitArgs;
    }
    else {
        Object[] argsToResolve = null;
        synchronized (mbd.constructorArgumentLock) {
            constructorToUse = (Constructor<?>) mbd.resolvedConstructorOrFactoryMethod;
            if (constructorToUse != null && mbd.constructorArgumentsResolved) {
                // Found a cached constructor...
                argsToUse = mbd.resolvedConstructorArguments;
                if (argsToUse == null) {
                    argsToResolve = mbd.preparedConstructorArguments;
                }
            }
        }
        if (argsToResolve != null) {
            argsToUse = resolvePreparedArguments(beanName, mbd, bw, constructorToUse, argsToResolve);
        }
    }

    if (constructorToUse == null) {
        // Need to resolve the constructor.
        boolean autowiring = (chosenCtors != null ||
                mbd.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_CONSTRUCTOR);
        ConstructorArgumentValues resolvedValues = null;

        int minNrOfArgs;
        if (explicitArgs != null) {
            minNrOfArgs = explicitArgs.length;
        }
        else {
            //获取构造函数的参数
            ConstructorArgumentValues cargs = mbd.getConstructorArgumentValues();
            resolvedValues = new ConstructorArgumentValues();
            minNrOfArgs = resolveConstructorArguments(beanName, mbd, bw, cargs, resolvedValues);
        }

        // Take specified constructors, if any.
        Constructor<?>[] candidates = chosenCtors;
        if (candidates == null) {
            //拿到class
            Class<?> beanClass = mbd.getBeanClass();
            try {
                //校验
                candidates = (mbd.isNonPublicAccessAllowed() ?
                        beanClass.getDeclaredConstructors() : beanClass.getConstructors());
            }
            catch (Throwable ex) {
                throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                        "Resolution of declared constructors on bean Class [" + beanClass.getName() +
                                "] from ClassLoader [" + beanClass.getClassLoader() + "] failed", ex);
            }
        }
        //把所有的构造方法排序
        //排序的规则是参数少->参数多
        AutowireUtils.sortConstructors(candidates);
        int minTypeDiffWeight = Integer.MAX_VALUE;
        Set<Constructor<?>> ambiguousConstructors = null;
        LinkedList<UnsatisfiedDependencyException> causes = null;
        //这里是去找到对应的构造方法
        //如果找不到会抛出异常
        for (int i = 0; i < candidates.length; i++) {
            Constructor<?> candidate = candidates[i];
            Class<?>[] paramTypes = candidate.getParameterTypes();

            if (constructorToUse != null && argsToUse.length > paramTypes.length) {
                // Already found greedy constructor that can be satisfied ->
                // do not look any further, there are only less greedy constructors left.
                break;
            }
            if (paramTypes.length < minNrOfArgs) {
                continue;
            }

            ArgumentsHolder argsHolder;
            if (resolvedValues != null) {
                try {
                    String[] paramNames = ConstructorPropertiesChecker.evaluate(candidate, paramTypes.length);
                    if (paramNames == null) {
                        ParameterNameDiscoverer pnd = this.beanFactory.getParameterNameDiscoverer();
                        if (pnd != null) {
                            paramNames = pnd.getParameterNames(candidate);
                        }
                    }
                    argsHolder = createArgumentArray(
                            beanName, mbd, resolvedValues, bw, paramTypes, paramNames, candidate, autowiring);
                }
                catch (UnsatisfiedDependencyException ex) {
                    if (this.beanFactory.logger.isTraceEnabled()) {
                        this.beanFactory.logger.trace(
                                "Ignoring constructor [" + candidate + "] of bean '" + beanName + "': " + ex);
                    }
                    // Swallow and try next constructor.
                    if (causes == null) {
                        causes = new LinkedList<UnsatisfiedDependencyException>();
                    }
                    causes.add(ex);
                    continue;
                }
            }
            else {
                // Explicit arguments given -> arguments length must match exactly.
                if (paramTypes.length != explicitArgs.length) {
                    continue;
                }
                argsHolder = new ArgumentsHolder(explicitArgs);
            }

            int typeDiffWeight = (mbd.isLenientConstructorResolution() ?
                    argsHolder.getTypeDifferenceWeight(paramTypes) : argsHolder.getAssignabilityWeight(paramTypes));
            // Choose this constructor if it represents the closest match.
            if (typeDiffWeight < minTypeDiffWeight) {
                constructorToUse = candidate;
                argsHolderToUse = argsHolder;
                argsToUse = argsHolder.arguments;
                minTypeDiffWeight = typeDiffWeight;
                ambiguousConstructors = null;
            }
            else if (constructorToUse != null && typeDiffWeight == minTypeDiffWeight) {
                if (ambiguousConstructors == null) {
                    ambiguousConstructors = new LinkedHashSet<Constructor<?>>();
                    ambiguousConstructors.add(constructorToUse);
                }
                ambiguousConstructors.add(candidate);
            }
        }

        if (constructorToUse == null) {
            if (causes != null) {
                UnsatisfiedDependencyException ex = causes.removeLast();
                for (Exception cause : causes) {
                    this.beanFactory.onSuppressedException(cause);
                }
                throw ex;
            }
            throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                    "Could not resolve matching constructor " +
                    "(hint: specify index/type/name arguments for simple parameters to avoid type ambiguities)");
        }
        else if (ambiguousConstructors != null && !mbd.isLenientConstructorResolution()) {
            throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                    "Ambiguous constructor matches found in bean '" + beanName + "' " +
                    "(hint: specify index/type/name arguments for simple parameters to avoid type ambiguities): " +
                    ambiguousConstructors);
        }

        if (explicitArgs == null) {
            argsHolderToUse.storeCache(mbd, constructorToUse);
        }
    }

    try {
        Object beanInstance;

        if (System.getSecurityManager() != null) {
            final Constructor<?> ctorToUse = constructorToUse;
            final Object[] argumentsToUse = argsToUse;
            beanInstance = AccessController.doPrivileged(new PrivilegedAction<Object>() {
                @Override
                public Object run() {
                    //实例化的关键代码。
                    return beanFactory.getInstantiationStrategy().instantiate(
                            mbd, beanName, beanFactory, ctorToUse, argumentsToUse);
                }
            }, beanFactory.getAccessControlContext());
        }
        else {
            beanInstance = this.beanFactory.getInstantiationStrategy().instantiate(
                    mbd, beanName, this.beanFactory, constructorToUse, argsToUse);
        }

        bw.setWrappedInstance(beanInstance);
        return bw;
    }
    catch (Throwable ex) {
        throw new BeanCreationException(mbd.getResourceDescription(), beanName,
                "Bean instantiation via constructor failed", ex);
    }
}
```
### instantiateBean
```java
protected BeanWrapper instantiateBean(final String beanName, final RootBeanDefinition mbd) {
    try {
        Object beanInstance;
        final BeanFactory parent = this;
        if (System.getSecurityManager() != null) {
            beanInstance = AccessController.doPrivileged(new PrivilegedAction<Object>() {
                @Override
                public Object run() {
                    return getInstantiationStrategy().instantiate(mbd, beanName, parent);
                }
            }, getAccessControlContext());
        }
        else {
            beanInstance = getInstantiationStrategy().instantiate(mbd, beanName, parent);
        }
        BeanWrapper bw = new BeanWrapperImpl(beanInstance);
        initBeanWrapper(bw);
        return bw;
    }
    catch (Throwable ex) {
        throw new BeanCreationException(mbd.getResourceDescription(), beanName, "Instantiation of bean failed", ex);
    }
}
```
```java
public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner) {
    // Don't override the class with CGLIB if no overrides.
    if (beanDefinition.getMethodOverrides().isEmpty()) {
        Constructor<?> constructorToUse;
        synchronized (beanDefinition.constructorArgumentLock) {
            constructorToUse = (Constructor<?>) beanDefinition.resolvedConstructorOrFactoryMethod;
            if (constructorToUse == null) {
                final Class clazz = beanDefinition.getBeanClass();
                if (clazz.isInterface()) {
                    throw new BeanInstantiationException(clazz, "Specified class is an interface");
                }
                try {
                    if (System.getSecurityManager() != null) {
                        constructorToUse = AccessController.doPrivileged(new PrivilegedExceptionAction<Constructor>() {
                            public Constructor run() throws Exception {
                                return clazz.getDeclaredConstructor((Class[]) null);
                            }
                        });
                    }
                    else {
                        constructorToUse = clazz.getDeclaredConstructor((Class[]) null);
                    }
                    beanDefinition.resolvedConstructorOrFactoryMethod = constructorToUse;
                }
                catch (Exception ex) {
                    throw new BeanInstantiationException(clazz, "No default constructor found", ex);
                }
            }
        }
        return BeanUtils.instantiateClass(constructorToUse);
    }
    else {
        // Must generate CGLIB subclass.
        
        return instantiateWithMethodInjection(beanDefinition, beanName, owner);
    }
}
```
整段代码都在做一件事情，就是选择一个使用的构造函数。然后BeanUtils.instantiateClass(constructorToUse)实例化一个实例
```java
public static <T> T instantiateClass(Constructor<T> ctor, Object... args) throws BeanInstantiationException {
    Assert.notNull(ctor, "Constructor must not be null");
    try {
        ReflectionUtils.makeAccessible(ctor);
        return ctor.newInstance(args);
    }
    catch (InstantiationException ex) {
        throw new BeanInstantiationException(ctor.getDeclaringClass(),
                "Is it an abstract class?", ex);
    }
    catch (IllegalAccessException ex) {
        throw new BeanInstantiationException(ctor.getDeclaringClass(),
                "Is the constructor accessible?", ex);
    }
    catch (IllegalArgumentException ex) {
        throw new BeanInstantiationException(ctor.getDeclaringClass(),
                "Illegal arguments for constructor", ex);
    }
    catch (InvocationTargetException ex) {
        throw new BeanInstantiationException(ctor.getDeclaringClass(),
                "Constructor threw exception", ex.getTargetException());
    }
}
```
通过反射生成Bean的实例。看到前面有一步makeAccessible，这意味着即使Bean的构造函数是private、protected的，依然不影响Bean的构造。注意一下，这里被实例化出来的Bean并不会直接返回，而是会被包装为BeanWrapper继续在后面使用。
# AOP标签解析类AopNamespaceHandler
```

public void init() {
    // In 2.0 XSD as well as in 2.1 XSD.
    // IOC 介绍了在解析扩展的标签是是根据namespaceHandler的进行解析，
    // 1.首先会调用NamespaceHandler的init方法，init方法会这里就注入了对应的标签解析器，
    // 2.然后会调用NamespaceHandler的parse方法。进行解析。
    // 3.这里分别针对aop:config,aop:aspectj-autoproxy,aop:scoped-proxy,aop:spring-configured的解析器。
    registerBeanDefinitionParser("config", new ConfigBeanDefinitionParser());
    registerBeanDefinitionParser("aspectj-autoproxy", new AspectJAutoProxyBeanDefinitionParser());
    registerBeanDefinitionDecorator("scoped-proxy", new ScopedProxyBeanDefinitionDecorator());

    // Only in 2.0 XSD: moved to context namespace as of 2.1
    registerBeanDefinitionParser("spring-configured", new SpringConfiguredBeanDefinitionParser());
}
```

栗子：
```
    <aop:config>
        <aop:aspect id="countAgeCalls" ref="countingAdvice">
            <aop:pointcut id="pc" expression="execution(* getAge())"/>
            <aop:before pointcut-ref="pc" method="myBeforeAdvice" />
            <aop:after pointcut-ref="pc" method="myAfterAdvice" />
            <aop:after-returning pointcut-ref="pc" method="myAfterReturningAdvice" returning="age"/>
            <aop:after-throwing pointcut-ref="pc" method="myAfterThrowingAdvice" throwing="ex"/>
            <aop:around pointcut-ref="pc" method="myAroundAdvice"/>
        </aop:aspect>
    </aop:config>

    <bean id="getNameCounter" class="org.springframework.aop.framework.CountingBeforeAdvice"/>

    <bean id="getAgeCounter" class="org.springframework.aop.framework.CountingBeforeAdvice"/>

    <bean id="testBean" class="org.springframework.tests.sample.beans.TestBean"/>

    <bean id="countingAdvice" class="org.springframework.aop.config.CountingAspectJAdvice"/>
```
我们来仔细看看具体的解析过程，看看spring aop最终会给spring注入什么bean？

最外层是aop:config 那么最终会调用ConfigBeanDefinitionParser.parse方法：
# ConfigBeanDefinitionParser.parse
```java
public BeanDefinition parse(Element element, ParserContext parserContext) {
        //tagName=aop:config 
        //parserContext.extractSource(element)= null;
        //组合组件定义
        CompositeComponentDefinition compositeDef =
                new CompositeComponentDefinition(element.getTagName(), parserContext.extractSource(element));
        //放入到stack 栈中。
        parserContext.pushContainingComponent(compositeDef);
        // 创建内部自动代理创建者，这里会注册一个AspectJAwareAdvisorAutoProxyCreator到spring 容器里
        // 同时会给上面定义的组件加入内嵌组件信息：其中内嵌组件信息是
        BeanbeanDefinition中class为AspectJAwareAdvisorAutoProxyCreator，beanName为：org.springframework.aop.config.internalAutoProxyCreator
        configureAutoProxyCreator(parserContext, element);

        //获取子节点
        List<Element> childElts = DomUtils.getChildElements(element);
        for (Element elt: childElts) {
            String localName = parserContext.getDelegate().getLocalName(elt);
            // 如果是pointcut 
            if (POINTCUT.equals(localName)) {
                parsePointcut(elt, parserContext);
            }
            //如果是advisor
            else if (ADVISOR.equals(localName)) {
                parseAdvisor(elt, parserContext);
            }
            //如果是Aspect
            else if (ASPECT.equals(localName)) {
                parseAspect(elt, parserContext);
            }
        }
        //从栈里弹出来并进行注册
        parserContext.popAndRegisterContainingComponent();
        return null;
    }
```

上面的栗子，子节点是aspect会进入到parseAspect


```java
//<aop:aspect id="countAgeCalls" ref="countingAdvice">
//解析如上标签
private void parseAspect(Element aspectElement, ParserContext parserContext) {
        //这里就会获取到aspectId=countAgeCalls ，aspectName=countingAdvice
        String aspectId = aspectElement.getAttribute(ID);
        String aspectName = aspectElement.getAttribute(REF);

        try {
            //把ID和Name暂存到解析状态栈里
            this.parseState.push(new AspectEntry(aspectId, aspectName));
            //定义一个beanDefinitions列表用来存aspect子标签下的信息
            List<BeanDefinition> beanDefinitions = new ArrayList<BeanDefinition>();
            //定义一个bean的引用列表，也是用来存spect子标签下的信息
            List<BeanReference> beanReferences = new ArrayList<BeanReference>();
            // 读取declare-parents子节点。
            // decalare-parents标签能够给某个接口进行接口的扩展。给接口添加新的功能。    
            List<Element> declareParents = DomUtils.getChildElementsByTagName(aspectElement, DECLARE_PARENTS);
            for (int i = METHOD_INDEX; i < declareParents.size(); i++) {
                Element declareParentsElement = declareParents.get(i);
                beanDefinitions.add(parseDeclareParents(declareParentsElement, parserContext));
            }

            // We have to parse "advice" and all the advice kinds in one loop, to get the
            // ordering semantics right.
            
            //解析完declareParents之后，解析aspect下所有子节点
            NodeList nodeList = aspectElement.getChildNodes();
            boolean adviceFoundAlready = false;
            for (int i = 0; i < nodeList.getLength(); i++) {
                Node node = nodeList.item(i);
                //1.判断是不是before，after，after-return，after-throwing，around标签，
                //2.所以pointcut标签这里是不会被处理的。
                if (isAdviceNode(node, parserContext)) {
                    //adviceFoundAlready 是为了防止重复添加
                    if (!adviceFoundAlready) {
                        adviceFoundAlready = true;
                        if (!StringUtils.hasText(aspectName)) {
                            parserContext.getReaderContext().error(
                                    "<aspect> tag needs aspect bean reference via 'ref' attribute when declaring advices.",
                                    aspectElement, this.parseState.snapshot());
                            return;
                        }
                        // 把aspectName为countingAdvice的bean添加到引用列表
                        beanReferences.add(new RuntimeBeanReference(aspectName));
                    }
                    //生成一个advice 的bean信息，
                    //这个方法，封装了很多IOC相关信息在里头。会抽出来详细分析一下。
                    AbstractBeanDefinition advisorDefinition = parseAdvice(
                            aspectName, i, aspectElement, (Element) node, parserContext, beanDefinitions, beanReferences);
                    //注册到bean信息表中，
                    beanDefinitions.add(advisorDefinition);
                }
            }
            // 创建一个组合Definition，同时把包含的advicor信息，和对应ref信息存起来
            AspectComponentDefinition aspectComponentDefinition = createAspectComponentDefinition(
                    aspectElement, aspectId, beanDefinitions, beanReferences, parserContext);
            //压入组合定义栈。
            parserContext.pushContainingComponent(aspectComponentDefinition);
            //解析 pointcuts标签。
            List<Element> pointcuts = DomUtils.getChildElementsByTagName(aspectElement, POINTCUT);
            for (Element pointcutElement : pointcuts) {
                //这里会生成POINTCUT相关bean信息，
                parsePointcut(pointcutElement, parserContext);
            }

            parserContext.popAndRegisterContainingComponent();
        }
        finally {
            this.parseState.pop();
        }
    }
```
## parseAdvice
```java 
private AbstractBeanDefinition parseAdvice(
            String aspectName, int order, Element aspectElement, Element adviceElement, ParserContext parserContext,
            List<BeanDefinition> beanDefinitions, List<BeanReference> beanReferences) {

        try {

            this.parseState.push(new AdviceEntry(
            //拿到adivce标签    
            parserContext.getDelegate().getLocalName(adviceElement)));

            // create the method factory bean
            // 创建一个FactoryBean<Method> 的bean的实例MethodLocatingFactoryBean
            // 同时给MethodLocatingFactoryBean 中的targetBeanName和methodName属性值进行赋值。
            // MethodLocatingFactoryBean 会根据从beanFactory中通过targetBeanName拿到bean，通过反射，获取到对应的方法。同时IOC通过factoryBean的getObject方法就能获取到对应的method信息了。

            RootBeanDefinition methodDefinition = new RootBeanDefinition(MethodLocatingFactoryBean.class);
            methodDefinition.getPropertyValues().add("targetBeanName", aspectName);
            methodDefinition.getPropertyValues().add("methodName", adviceElement.getAttribute("method"));
            //这句是告诉IOC容器这个是一个基础bean，不是应用程序定义的bean
            methodDefinition.setSynthetic(true);

            // create instance factory definition
            // 创建一个切面AspectInstanceFactory，我们能够通过，getAspectInstance获取到对应的bean。
            // 为嘛要创建工厂？因为在构建adivce的时候需要使用到。
            RootBeanDefinition aspectFactoryDef =
                    new RootBeanDefinition(SimpleBeanFactoryAwareAspectInstanceFactory.class);
            aspectFactoryDef.getPropertyValues().add("aspectBeanName", aspectName);
            aspectFactoryDef.setSynthetic(true);

            // register the pointcut
            // 这里会注册advice信息，会根据类型来注册advice，
            AbstractBeanDefinition adviceDef = createAdviceDefinition(
                    adviceElement, parserContext, aspectName, order, methodDefinition, aspectFactoryDef,
                    beanDefinitions, beanReferences);

            // configure the advisor
            RootBeanDefinition advisorDefinition = new RootBeanDefinition(AspectJPointcutAdvisor.class);
            //source 是null
            advisorDefinition.setSource(parserContext.extractSource(adviceElement));
            //通过构造函数把 advice信息注入到advisor里面。
            advisorDefinition.getConstructorArgumentValues().addGenericArgumentValue(adviceDef);
            if (aspectElement.hasAttribute(ORDER_PROPERTY)) {
                advisorDefinition.getPropertyValues().add(
                        ORDER_PROPERTY, aspectElement.getAttribute(ORDER_PROPERTY));
            }

            // register the final advisor
            // 把advisor bean 信息注册
            // name=org.springframework.aop.aspectj.AspectJPointcutAdvisor#0 
            parserContext.getReaderContext().registerWithGeneratedName(advisorDefinition);
            //同时返回 advisor的信息。
            return advisorDefinition;
        }
        finally {
            this.parseState.pop();
        }
```

以上这个例子最终会注册一个组合组件信息：
如下图所示。
![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1560677098822.png)
这样aop解析就算处理完了。

解析完了，那么代理类到底是如何生成的呢？

其实就是通过AspectJAwareAdvisorAutoProxyCreator这个类实例化的时候。如何找到入口呢？
那么我们就需要复习一下IOC里的内容了，在IOC介绍里，知道bean的实例化是在AbstractAutowireCapableBeanFactory.createBean方法。

```java 
protected Object createBean(String beanName, RootBeanDefinition mbd, Object[] args) throws BeanCreationException {
        if (logger.isDebugEnabled()) {
            logger.debug("Creating instance of bean '" + beanName + "'");
        }
        RootBeanDefinition mbdToUse = mbd;

        // Make sure bean class is actually resolved at this point, and
        // clone the bean definition in case of a dynamically resolved Class
        // which cannot be stored in the shared merged bean definition.
        Class<?> resolvedClass = resolveBeanClass(mbd, beanName);
        if (resolvedClass != null && !mbd.hasBeanClass() && mbd.getBeanClassName() != null) {
            mbdToUse = new RootBeanDefinition(mbd);
            mbdToUse.setBeanClass(resolvedClass);
        }

        // Prepare method overrides.
        try {
            mbdToUse.prepareMethodOverrides();
        }
        catch (BeanDefinitionValidationException ex) {
            throw new BeanDefinitionStoreException(mbdToUse.getResourceDescription(),
                    beanName, "Validation of method overrides failed", ex);
        }

        try {
            // Give BeanPostProcessors a chance to return a proxy instead of the target bean instance.
            //这里会允许BeanPostProcessors去修改对应的bean信息，使之返回对应的目标bean的代理对象。
            //判断条件是：!mbd.isSynthetic() && hasInstantiationAwareBeanPostProcessors()，这就是为什么要前面要setSynthetic的原因了，因为处理beanPostProcessor会作校验。
            
            Object bean = resolveBeforeInstantiation(beanName, mbdToUse);
            if (bean != null) {
                return bean;
            }
        }
        catch (Throwable ex) {
            throw new BeanCreationException(mbdToUse.getResourceDescription(), beanName,
                    "BeanPostProcessor before instantiation of bean failed", ex);
        }

        Object beanInstance = doCreateBean(beanName, mbdToUse, args);
        if (logger.isDebugEnabled()) {
            logger.debug("Finished creating instance of bean '" + beanName + "'");
        }
        return beanInstance;
    }
```
## resolveBeforeInstantiation 
```java 
protected Object resolveBeforeInstantiation(String beanName, RootBeanDefinition mbd) {
        Object bean = null;
        if (!Boolean.FALSE.equals(mbd.beforeInstantiationResolved)) {
            // Make sure bean class is actually resolved at this point.
            if (!mbd.isSynthetic() && hasInstantiationAwareBeanPostProcessors()) {
                Class<?> targetType = determineTargetType(beanName, mbd);
                if (targetType != null) {
                    // 执行前置处理。
                    bean = applyBeanPostProcessorsBeforeInstantiation(targetType, beanName);
                    if (bean != null) {
                        // 执行后置处理
                        bean = applyBeanPostProcessorsAfterInitialization(bean, beanName);
                    }
                }
            }
            mbd.beforeInstantiationResolved = (bean != null);
        }
        return bean;
    }
```
## applyBeanPostProcessorsBeforeInstantiation

```java
protected Object applyBeanPostProcessorsBeforeInstantiation(Class<?> beanClass, String beanName)
            throws BeansException {

        for (BeanPostProcessor bp : getBeanPostProcessors()) {
            if (bp instanceof InstantiationAwareBeanPostProcessor) {
                InstantiationAwareBeanPostProcessor ibp = (InstantiationAwareBeanPostProcessor) bp;
                Object result = ibp.postProcessBeforeInstantiation(beanClass, beanName);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }
```
## applyBeanPostProcessorsAfterInitialization

```java
public Object applyBeanPostProcessorsAfterInitialization(Object existingBean, String beanName)
            throws BeansException {

        Object result = existingBean;
        for (BeanPostProcessor beanProcessor : getBeanPostProcessors()) {
            result = beanProcessor.postProcessAfterInitialization(result, beanName);
            if (result == null) {
                return result;
            }
        }
        return result;
    }
```
而 AspectJAwareAdvisorAutoProxyCreator 继承了AbstractAutoProxyCreator
而AbstractAutoProxyCreator 实现了SmartInstantiationAwareBeanPostProcessor
所以 AbstractAutoProxyCreator中的postProcessAfterInitialization方法和postProcessBeforeInstantiation会被调用

## postProcessBeforeInstantiation
```java
public Object postProcessBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException {
        Object cacheKey = getCacheKey(beanClass, beanName);

        if (beanName == null || !this.targetSourcedBeans.contains(beanName)) {
            if (this.advisedBeans.containsKey(cacheKey)) {
                return null;
            }
            //是不是基础bean，和是不是应该被跳过的类
            if (isInfrastructureClass(beanClass) || shouldSkip(beanClass, beanName)) {
                this.advisedBeans.put(cacheKey, Boolean.FALSE);
                return null;
            }
        }

        // Create proxy here if we have a custom TargetSource.
        // Suppresses unnecessary default instantiation of the target bean:
        // The TargetSource will handle target instances in a custom fashion.
        if (beanName != null) {
            TargetSource targetSource = getCustomTargetSource(beanClass, beanName);
            if (targetSource != null) {
                this.targetSourcedBeans.add(beanName);
                //找到所有的advisor
                Object[] specificInterceptors = getAdvicesAndAdvisorsForBean(beanClass, beanName, targetSource);
                //创建代理类
                Object proxy = createProxy(beanClass, beanName, specificInterceptors, targetSource);
                //同时把代理类缓存起来
                this.proxyTypes.put(cacheKey, proxy.getClass());
                return proxy;
            }
        }

        return null;
    }
```

这样我们找到AOP代理类创建的入口了
# 代理类的创建
## createProxy
```java
protected Object createProxy(
            Class<?> beanClass, String beanName, Object[] specificInterceptors, TargetSource targetSource) {

        if (this.beanFactory instanceof ConfigurableListableBeanFactory) {
            AutoProxyUtils.exposeTargetClass((ConfigurableListableBeanFactory) this.beanFactory, beanName, beanClass);
        }
        //创建代理工厂
        ProxyFactory proxyFactory = new ProxyFactory();
        proxyFactory.copyFrom(this);
        // 如果是类模式ProxyTargetClass为false
        if (!proxyFactory.isProxyTargetClass()) {
            // 判断是不是需要动态代理的类，
            // 判断的条件是return Boolean.TRUE.equals(bd.getAttribute(PRESERVE_TARGET_CLASS_ATTRIBUTE));
            if (shouldProxyTargetClass(beanClass, beanName)) {
                proxyFactory.setProxyTargetClass(true);
            }
            else {
                //否则就是用接口代理的方式，
                //先去获取所有的接口，过滤一些Aware接口，InitializingBean，DisposableBean接口，
                //如果还有别的接口且接口中有方法，就用接口代理模式 否则就切换会类代理模式
                evaluateProxyInterfaces(beanClass, proxyFactory);
            }
        }
        // 这里获取所有的advisors
        // Aop的配置有多种多样，有通过interceptorNames进行配置的。
        Advisor[] advisors = buildAdvisors(beanName, specificInterceptors);
        for (Advisor advisor : advisors) {
            //把所有的advisor加入代理工厂
            proxyFactory.addAdvisor(advisor);
        }
        // 把需要代理的目标对象也加入到工厂里
        proxyFactory.setTargetSource(targetSource);
        // 自定义工厂配置，空方法，供子类继承实现
        customizeProxyFactory(proxyFactory);

        proxyFactory.setFrozen(this.freezeProxy);
        if (advisorsPreFiltered()) {
            proxyFactory.setPreFiltered(true);
        }
        //返回代理对象
        // 这里由AopProxyFactory实现类DefaultAopProxyFactory.
        return proxyFactory.getProxy(getProxyClassLoader());
    }
```
## DefaultAopProxyFactory.createAopProxy
```java
@Override
    public AopProxy createAopProxy(AdvisedSupport config) throws AopConfigException {
        if (config.isOptimize() || config.isProxyTargetClass() || hasNoUserSuppliedProxyInterfaces(config)) {
            Class<?> targetClass = config.getTargetClass();
            if (targetClass == null) {
                throw new AopConfigException("TargetSource cannot determine target class: " +
                        "Either an interface or a target is required for proxy creation.");
            }
            if (targetClass.isInterface() || Proxy.isProxyClass(targetClass)) {
                return new JdkDynamicAopProxy(config);
            }
            return new ObjenesisCglibAopProxy(config);
        }
        else {
            return new JdkDynamicAopProxy(config);
        }
    }
```
这里可以清楚的看见JDK动态代理和，CGLIB动态代理了，是不是比较清楚了，默认是jdk动态代理。

## getProxy接口实现
### jdk动态代理实现
```java
@Override
    public Object getProxy(ClassLoader classLoader) {
        if (logger.isDebugEnabled()) {
            logger.debug("Creating JDK dynamic proxy: target source is " + this.advised.getTargetSource());
        }
        Class<?>[] proxiedInterfaces = AopProxyUtils.completeProxiedInterfaces(this.advised);
        findDefinedEqualsAndHashCodeMethods(proxiedInterfaces);
        return Proxy.newProxyInstance(classLoader, proxiedInterfaces, this);
    }
```
因为 JdkDynamicAopProxy实现了InvocationHandler，所以会调用invoke方法

```java
@Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        MethodInvocation invocation;
        Object oldProxy = null;
        boolean setProxyContext = false;

        TargetSource targetSource = this.advised.targetSource;
        Class<?> targetClass = null;
        Object target = null;

        try {
            if (!this.equalsDefined && AopUtils.isEqualsMethod(method)) {
                // The target does not implement the equals(Object) method itself.
                return equals(args[0]);
            }
            if (!this.hashCodeDefined && AopUtils.isHashCodeMethod(method)) {
                // The target does not implement the hashCode() method itself.
                return hashCode();
            }
            if (!this.advised.opaque && method.getDeclaringClass().isInterface() &&
                    method.getDeclaringClass().isAssignableFrom(Advised.class)) {
                // Service invocations on ProxyConfig with the proxy config...
                // 反射调用
                return AopUtils.invokeJoinpointUsingReflection(this.advised, method, args);
            }

            Object retVal;
            //代理类是否导出来暴露给应用
            if (this.advised.exposeProxy) {
                // Make invocation available if necessary.
                oldProxy = AopContext.setCurrentProxy(proxy);
                setProxyContext = true;
            }

            // May be null. Get as late as possible to minimize the time we "own" the target,
            // in case it comes from a pool.
            // 取出真实代理的对象
            target = targetSource.getTarget();
            if (target != null) {
                targetClass = target.getClass();
            }

            // Get the interception chain for this method.
            // 获取拦截器链
            // 所有的增强方法都是通过拦截器链实现的。
            List<Object> chain = 
            this.advised.getInterceptorsAndDynamicInterceptionAdvice(method, targetClass);

            // Check whether we have any advice. If we don't, we can fallback on direct
            // reflective invocation of the target, and avoid creating a MethodInvocation.
            // 如果没有拦截器链，就直接反射调用目标方法。
            if (chain.isEmpty()) {
                // We can skip creating a MethodInvocation: just invoke the target directly
                // Note that the final invoker must be an InvokerInterceptor so we know it does
                // nothing but a reflective operation on the target, and no hot swapping or fancy proxying.
                Object[] argsToUse = AopProxyUtils.adaptArgumentsIfNecessary(method, args);
                retVal = AopUtils.invokeJoinpointUsingReflection(target, method, argsToUse);
            }
            else {
                // We need to create a method invocation...
                // 否则先执行拦截器链中的方法。
                invocation = new ReflectiveMethodInvocation(proxy, target, method, args, targetClass, chain);
                // Proceed to the joinpoint through the interceptor chain.
                retVal = invocation.proceed();
            }

            // Massage return value if necessary.
            Class<?> returnType = method.getReturnType();
            if (retVal != null && retVal == target && returnType.isInstance(proxy) &&
                    !RawTargetAccess.class.isAssignableFrom(method.getDeclaringClass())) {
                // Special case: it returned "this" and the return type of the method
                // is type-compatible. Note that we can't help if the target sets
                // a reference to itself in another returned object.
                retVal = proxy;
            }
            else if (retVal == null && returnType != Void.TYPE && returnType.isPrimitive()) {
                throw new AopInvocationException(
                        "Null return value from advice does not match primitive return type for: " + method);
            }
            return retVal;
        }
        finally {
            if (target != null && !targetSource.isStatic()) {
                // Must have come from TargetSource.
                targetSource.releaseTarget(target);
            }
            if (setProxyContext) {
                // Restore old proxy.
                AopContext.setCurrentProxy(oldProxy);
            }
        }
    }
```
### CGLIB 动态代理实现
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

## 拦截器链创建

### jdk动态代理 getInterceptorsAndDynamicInterceptionAdvice
```java
public List<Object> getInterceptorsAndDynamicInterceptionAdvice(Method method, Class<?> targetClass) {
        MethodCacheKey cacheKey = new MethodCacheKey(method);
        List<Object> cached = this.methodCache.get(cacheKey);
        // 这里作了缓存供下次再次使用
        if (cached == null) {
            //AdvisorChainFactory advisorChainFactory = new DefaultAdvisorChainFactory();
            // 拦截器链是由DefaultAdvisorChainFactory来获取的
            cached = this.advisorChainFactory.getInterceptorsAndDynamicInterceptionAdvice(
                    this, method, targetClass);
            this.methodCache.put(cacheKey, cached);
        }
        return cached;
    }
```
```java
public List<Object> getInterceptorsAndDynamicInterceptionAdvice(
            Advised config, Method method, Class<?> targetClass) {

        // This is somewhat tricky... We have to process introductions first,
        // but we need to preserve order in the ultimate list.
        List<Object> interceptorList = new ArrayList<Object>(config.getAdvisors().length);
        Class<?> actualClass = (targetClass != null ? targetClass : method.getDeclaringClass());
        boolean hasIntroductions = hasMatchingIntroductions(config, actualClass);
        // 这里会获取到DefaultAdvisorAdapterRegistry一个实例，
        // 这个是拦截器的一个适配器实现工厂，里面集成了MethodBeforeAdviceAdapter，AfterReturningAdviceAdapter，ThrowsAdviceAdapter。
        AdvisorAdapterRegistry registry = GlobalAdvisorAdapterRegistry.getInstance();
        //遍历所有的advisor
        for (Advisor advisor : config.getAdvisors()) {
            // 是不是切面通知
            if (advisor instanceof PointcutAdvisor) {
                // Add it conditionally.
                PointcutAdvisor pointcutAdvisor = (PointcutAdvisor) advisor;
                if (config.isPreFiltered() || pointcutAdvisor.getPointcut().getClassFilter().matches(actualClass)) {
                    MethodInterceptor[] interceptors = registry.getInterceptors(advisor);
                    MethodMatcher mm = pointcutAdvisor.getPointcut().getMethodMatcher();
                    //使用MethodMatchers中的macher方法对Method作匹配过滤
                    if (MethodMatchers.matches(mm, method, actualClass, hasIntroductions)) {
                        if (mm.isRuntime()) {
                            // Creating a new object instance in the getInterceptors() method
                            // isn't a problem as we normally cache created chains.
                            for (MethodInterceptor interceptor : interceptors) {
                                interceptorList.add(new InterceptorAndDynamicMethodMatcher(interceptor, mm));
                            }
                        }
                        else {
                            interceptorList.addAll(Arrays.asList(interceptors));
                        }
                    }
                }
            }
            // 引介
            else if (advisor instanceof IntroductionAdvisor) {
                IntroductionAdvisor ia = (IntroductionAdvisor) advisor;
                if (config.isPreFiltered() || ia.getClassFilter().matches(actualClass)) {
                    Interceptor[] interceptors = registry.getInterceptors(advisor);
                    interceptorList.addAll(Arrays.asList(interceptors));
                }
            }
            else {
                Interceptor[] interceptors = registry.getInterceptors(advisor);
                interceptorList.addAll(Arrays.asList(interceptors));
            }
        }

        return interceptorList;
    }
```

## DefaultAdvisorAdapterRegistry

```java
public DefaultAdvisorAdapterRegistry() {
    //注册了三个通知适配者     
    registerAdvisorAdapter(new MethodBeforeAdviceAdapter());
    registerAdvisorAdapter(new AfterReturningAdviceAdapter());
    registerAdvisorAdapter(new ThrowsAdviceAdapter());
}
```
## getInterceptors

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
## 拦截器链的执行

ReflectiveMethodInvocation.proceed
```java
public Object proceed() throws Throwable {
        //  We start with an index of -1 and increment early.
        // 从-1开始，如果没有调用链，就直接反射调用目标类
        if (this.currentInterceptorIndex == this.interceptorsAndDynamicMethodMatchers.size() - 1) {
            return invokeJoinpoint();
        }

        Object interceptorOrInterceptionAdvice =
                this.interceptorsAndDynamicMethodMatchers.get(++this.currentInterceptorIndex);
        if (interceptorOrInterceptionAdvice instanceof InterceptorAndDynamicMethodMatcher) {
            // Evaluate dynamic method matcher here: static part will already have
            // been evaluated and found to match.
            // 这里会触发匹配，如果和定义的pointpcut匹配，那么这个advice将会被执行
            InterceptorAndDynamicMethodMatcher dm =
                    (InterceptorAndDynamicMethodMatcher) interceptorOrInterceptionAdvice;
            if (dm.methodMatcher.matches(this.method, this.targetClass, this.arguments)) {
                return dm.interceptor.invoke(this);
            }
            else {
                // Dynamic matching failed.
                // Skip this interceptor and invoke the next in the chain.
                // 如果不匹配，就会递归调用proceed，直到所有的拦截器都被执行完。
                return proceed();
            }
        }
        else {
            // It's an interceptor, so we just invoke it: The pointcut will have
            // been evaluated statically before this object was constructed.
            // 如果是一个拦截器，就直接执行invoke方法。
            return ((MethodInterceptor) interceptorOrInterceptionAdvice).invoke(this);
        }
    }
```

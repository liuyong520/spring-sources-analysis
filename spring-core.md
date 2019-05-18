\[TOC\]

这么多的源代码，这么多包，往往不知道从何处开始下手阅读的这个源代码。我们在接触spring的时候，首先介绍的都是按照IOC、MVC、AOP、这种顺序介绍的。

# ClassPathXmlApplicationContext

## 类的继承图谱

废话不多说，就从这个启动类开始看吧。

先看一下这个类的继承图谱

![](/assets/import.png)

Classpath应用上下文最顶层接口Beanfactory。Beanfactory就是springIOC的容器。

## 构造方法

```java
public ClassPathXmlApplicationContext(String[] configLocations, boolean refresh, ApplicationContext parent)
            throws BeansException {
        //最终会调用的是AbstractApplicationContext的构造方法    
        super(parent);
        //根据配置的路径生成classpath的加载路径

        setConfigLocations(configLocations);
        if (refresh) {
            //刷新容器完成容器的初始化工作
            refresh();
        }
    }
```

看一下AbstractApplicationContext 构造方法作的是什么事情

```java
public AbstractApplicationContext(ApplicationContext parent) {
    this();
    //设置父容器
    setParent(parent);
}
public AbstractApplicationContext() {
    //获取配置资源的解析器
    this.resourcePatternResolver = getResourcePatternResolver();
}

protected ResourcePatternResolver getResourcePatternResolver() {
    //直接new一个PathMatchingResourcePatternResolver解析器 等一下再看这个PathMatchingResourcePatternResolver
    return new PathMatchingResourcePatternResolver(this);
}
@Override
public void setParent(ApplicationContext parent) {
    this.parent = parent;
    if (parent != null) {
        //如果父容器不为空且是ConfigurableEnvironment就把环境合并在一起
        Environment parentEnvironment = parent.getEnvironment();
        if (parentEnvironment instanceof ConfigurableEnvironment) {
            getEnvironment().merge((ConfigurableEnvironment) parentEnvironment);
        }
    }
}
//getEnvironment方法来自于ConfigurableApplicationContext接口，源码很简单，如果为空就调用createEnvironment创建一个。AbstractApplicationContext.createEnvironment:
public ConfigurableEnvironment getEnvironment() {
    if (this.environment == null) {

        this.environment = createEnvironment();
    }
    return this.environment;
}
```

## setConfigLocations

```java
//此方法的目的在于将占位符(placeholder)解析成实际的地址。比如可以这么写: new ClassPathXmlApplicationContext("classpath:config.xml");那么classpath:就是需要被解析的

public void setConfigLocations(String... locations) {
    if (locations != null) {
        Assert.noNullElements(locations, "Config locations must not be null");
        this.configLocations = new String[locations.length];
        for (int i = 0; i < locations.length; i++) {
            //解析成classpath的路径
            this.configLocations[i] = resolvePath(locations[i]).trim();
        }
    }
    else {
        this.configLocations = null;
    }
}
protected String resolvePath(String path) {
    return getEnvironment().resolveRequiredPlaceholders(path);
}
```

## refresh

重点介绍这个refresh方法：

```java
@Override
public void refresh() throws BeansException, IllegalStateException {
    synchronized (this.startupShutdownMonitor) {
        //准备刷新容器这里干的是：初始化资源，初始化spring事件容器，验证一下系统环境配置是否正确 这个
        prepareRefresh();
        //刷新内部bean工厂，同时拿到内部工厂beanfactory。
        ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();
        //    
        prepareBeanFactory(beanFactory);

        try {
            // Allows post-processing of the bean factory in context subclasses.
            postProcessBeanFactory(beanFactory);

            // Invoke factory processors registered as beans in the context.
            invokeBeanFactoryPostProcessors(beanFactory);

            // Register bean processors that intercept bean creation.
            registerBeanPostProcessors(beanFactory);

            // Initialize message source for this context.
            initMessageSource();

            // Initialize event multicaster for this context.
            initApplicationEventMulticaster();

            // Initialize other special beans in specific context subclasses.
            onRefresh();

            // Check for listener beans and register them.
            registerListeners();

            // Instantiate all remaining (non-lazy-init) singletons.
            finishBeanFactoryInitialization(beanFactory);

            // Last step: publish corresponding event.
            finishRefresh();
        }

        catch (BeansException ex) {
            if (logger.isWarnEnabled()) {
                logger.warn("Exception encountered during context initialization - " +
                        "cancelling refresh attempt: " + ex);
            }

            // Destroy already created singletons to avoid dangling resources.
            destroyBeans();

            // Reset 'active' flag.
            cancelRefresh(ex);

            // Propagate exception to caller.
            throw ex;
        }

        finally {
            // Reset common introspection caches in Spring's core, since we
            // might not ever need metadata for singleton beans anymore...
            resetCommonCaches();
        }
    }
```

### prepareRefresh

看看prepareRefresh 这个方法：

```java
protected void prepareRefresh() {
    this.startupDate = System.currentTimeMillis();

    this.closed.set(false);
    this.active.set(true);

    if (logger.isInfoEnabled()) {
        logger.info("Refreshing " + this);
    }

    // 初始化properties配置信息，这个方法其实是个空方法，让子类去复写的。子类可以继承这个类，实现这个方法自行去加载properties配置
    initPropertySources();

    //验证环境配置的properties是否是require的
    //如果是key=value value 为空的话，
    //就会存到一个MissingRequiredPropertiesException（这是一个异常的集合）
    //类里，同时抛出MissingRequiredPropertiesException
    getEnvironment().validateRequiredProperties();

    //初始化spring事件的容器
    this.earlyApplicationEvents = new LinkedHashSet<ApplicationEvent>();
}
```

### obtainFreshBeanFactory

接着继续看：ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory\(\);

```java
protected ConfigurableListableBeanFactory obtainFreshBeanFactory() {
    //刷新内部工厂 为嘛要刷新？这里其实做了三部操作
    //1。关闭原来创建的的容器，同时释放bean对象资源
    //2.重新加载beans配置文件，存到DefaultListableBeanFactory 容器里
    refreshBeanFactory();
    //获取beanFactory的实例。这里是调用的AbstractXmlApplicationContext里面的    getBeanFactory获取到DefaultListableBeanFactory的实例。
    ConfigurableListableBeanFactory beanFactory = getBeanFactory();
    if (logger.isDebugEnabled()) {
        logger.debug("Bean factory for " + getDisplayName() + ": " + beanFactory);
    }
    return beanFactory;
}
```

继续看refreshBeanFactory：这个方法是它的父类AbstractRefreshableApplicationContext实现的。

```java
@Override
protected final void refreshBeanFactory() throws BeansException {
    //如果已经有beanfactory了就把所以的bean给销毁掉，同时关闭beanfactory
    if (hasBeanFactory()) {
        destroyBeans();
        closeBeanFactory();
    }
    try {
        //重新创建一个beanfactory。待会我们再分析这个bean factory
        DefaultListableBeanFactory beanFactory = createBeanFactory();
        beanFactory.setSerializationId(getId());
        customizeBeanFactory(beanFactory);
        //加载所有的工厂实例 这个方法在classpathApplicationContext中是由AbstractXmlApplicationContext实现的。加载配置文件将所有的bean以beanDefinition的描述存在DefaultListableBeanFactory这个IOC容器里
        loadBeanDefinitions(beanFactory);
        synchronized (this.beanFactoryMonitor) {
            this.beanFactory = beanFactory;
        }
    }
    catch (IOException ex) {
        throw new ApplicationContextException("I/O error parsing bean definition source for " + getDisplayName(), ex);
    }
}
```

### prepareBeanFactory

接着往下看prepareBeanFactory 方法很长。

```java
protected void prepareBeanFactory(ConfigurableListableBeanFactory beanFactory) {
    // 设置beanFactory的ClassLoader为当前的ClassLoader
    beanFactory.setBeanClassLoader(getClassLoader());
    // 设置表达式解析器(解析bean定义中的一些表达式)这里是spel表达式解析器
    beanFactory.setBeanExpressionResolver(new StandardBeanExpressionResolver(beanFactory.getBeanClassLoader()));
    // 添加属性编辑注册器(注册属性编辑器)，属性编辑器实际上是属性的类型转换器，编辑器注册器里面其实是map结构
    // 因为bean的属性配置都是字符串类型的 实例化的时候要将这些属性转换为实际类型
    beanFactory.addPropertyEditorRegistrar(new ResourceEditorRegistrar(this, getEnvironment()));

    //// 添加BeanPostProcessor(Bean后置处理器)：ApplicationContextAwareProcessor
    // 在BEAN初始化之前，调用ApplicationContextAwareProcessor的postProcessBeforeInitialization
    // postProcessBeforeInitialization有如下功能
    // 处理所有的Aware接口，进行如下操作：
    // 如果bean实现了EnvironmentAware接口，调用bean.setEnvironment
    // 如果bean实现了EmbeddedValueResolverAware接口，调用bean.setEmbeddedValueResolver
    // 如果bean实现了ResourceLoaderAware接口，调用bean.setResourceLoader
    // 如果bean实现了ApplicationEventPublisherAware接口，调用bean.setApplicationEventPublisher
    // 如果bean实现了MessageSourceAware接口，调用bean.setMessageSource
   // 如果bean实现了ApplicationContextAware接口，调用bean.setApplicationContext

    beanFactory.addBeanPostProcessor(new ApplicationContextAwareProcessor(this));
    //  取消ResourceLoaderAware
    // 、ApplicationEventPublisherAware
    // 、MessageSourceAware
    // 、ApplicationContextAware
    // 、EnvironmentAware这5个接口的自动注入
    // 因为ApplicationContextAwareProcessor把这5个接口的实现工作做了
    beanFactory.ignoreDependencyInterface(ResourceLoaderAware.class);
    beanFactory.ignoreDependencyInterface(ApplicationEventPublisherAware.class);
    beanFactory.ignoreDependencyInterface(MessageSourceAware.class);
    beanFactory.ignoreDependencyInterface(ApplicationContextAware.class);
    beanFactory.ignoreDependencyInterface(EnvironmentAware.class);

    // 注入一些特殊的bean，不需要在bean文件里面定义。
    beanFactory.registerResolvableDependency(BeanFactory.class, beanFactory);
    beanFactory.registerResolvableDependency(ResourceLoader.class, this);
    beanFactory.registerResolvableDependency(ApplicationEventPublisher.class, this);
    beanFactory.registerResolvableDependency(ApplicationContext.class, this);

    // 检查容器中是否包含名称为loadTimeWeaver的bean，实际上是增加Aspectj的支持
    // AspectJ采用编译期织入、类加载期织入两种方式进行切面的织入
    // 类加载期织入简称为LTW（Load Time Weaving）,通过特殊的类加载器来代理JVM默认的类加载器实现
    if (beanFactory.containsBean(LOAD_TIME_WEAVER_BEAN_NAME)) {
        // 添加BEAN后置处理器：LoadTimeWeaverAwareProcessor
        // 在BEAN初始化之前检查BEAN是否实现了LoadTimeWeaverAware接口，
        // 如果是，则进行加载时织入，即静态代理。
        beanFactory.addBeanPostProcessor(new LoadTimeWeaverAwareProcessor(beanFactory));
        //设置特殊的类加载器
        beanFactory.setTempClassLoader(new ContextTypeMatchClassLoader(beanFactory.getBeanClassLoader()));
    }

    // 注册环境的environment bean
    if (!beanFactory.containsLocalBean(ENVIRONMENT_BEAN_NAME)) {
        beanFactory.registerSingleton(ENVIRONMENT_BEAN_NAME, getEnvironment());
    }
    //注册systemProperties的bean 其实就是map 
    if (!beanFactory.containsLocalBean(SYSTEM_PROPERTIES_BEAN_NAME)) {
        beanFactory.registerSingleton(SYSTEM_PROPERTIES_BEAN_NAME, getEnvironment().getSystemProperties());
    }
    注册系统环境bean，其实就是map
    if (!beanFactory.containsLocalBean(SYSTEM_ENVIRONMENT_BEAN_NAME)) {
        beanFactory.registerSingleton(SYSTEM_ENVIRONMENT_BEAN_NAME, getEnvironment().getSystemEnvironment());
    }
}
```

看看ApplicationContextAwareProcessor 的postProcessBeforeInitialization这个方法,看完这个方法就知道上面为嘛写这么多东西了

```java
public Object postProcessBeforeInitialization(final Object bean, String beanName) throws BeansException {
    AccessControlContext acc = null;
    //如果bean 实现了EmbeddedValueResolverAware、ResourceLoaderAware、
    //ApplicationEventPublisherAware、ApplicationContextAware接口。

    if (System.getSecurityManager() != null &&
            (bean instanceof EnvironmentAware || bean instanceof EmbeddedValueResolverAware ||
                    bean instanceof ResourceLoaderAware || bean instanceof ApplicationEventPublisherAware ||
                    bean instanceof MessageSourceAware || bean instanceof ApplicationContextAware)) {
        //获取权限控制上下文
        acc = this.applicationContext.getBeanFactory().getAccessControlContext();
    }
    //权限控制上下文非空
    if (acc != null) {
        //用权限控制器去调invokeAwareInterfaces方法
        AccessController.doPrivileged(new PrivilegedAction<Object>() {
            @Override
            public Object run() {
                invokeAwareInterfaces(bean);
                return null;
            }
        }, acc);
    }
    else {
        //否则就直接调用了
        invokeAwareInterfaces(bean);
    }

    return bean;
}
```

上面的方法始终都会调用invokeAwareInterfaces这个方法。

```java
private void invokeAwareInterfaces(Object bean) {
    if (bean instanceof Aware) {
        if (bean instanceof EnvironmentAware) {
            //setEnvironment
            ((EnvironmentAware) bean).setEnvironment(this.applicationContext.getEnvironment());
        }
        if (bean instanceof EmbeddedValueResolverAware) {
            //调用setEmbeddedValueResolver
            ((EmbeddedValueResolverAware) bean).setEmbeddedValueResolver(
                    new EmbeddedValueResolver(this.applicationContext.getBeanFactory()));
        }
        if (bean instanceof ResourceLoaderAware) {
            //调用setResourceLoader
            ((ResourceLoaderAware) bean).setResourceLoader(this.applicationContext);
        }
        if (bean instanceof ApplicationEventPublisherAware) {
            //调用setApplicationEventPublisher
            ((ApplicationEventPublisherAware) bean).setApplicationEventPublisher(this.applicationContext);
        }
        if (bean instanceof MessageSourceAware) {
            //调用setMessageSource
            ((MessageSourceAware) bean).setMessageSource(this.applicationContext);
        }
        if (bean instanceof ApplicationContextAware) {
            //调用setApplicationContext
            ((ApplicationContextAware) bean).setApplicationContext(this.applicationContext);
        }
    }
}
```

同理LoadTimeWeaverAwareProcessor里面实现也可以从postProcessBeforeInitialization的方法。这里就不介绍了。

### postProcessBeanFactory

继续介绍refresh方法里的方法postProcessBeanFactory\(beanFactory\);进去一看，一个未实现的空方法。干嘛用的？这个spring的提供的扩展，如果我们需要在容器所有bean定义被加载未实例化之前，我们可以注册一些BeanPostProcessors来实现在一些bean实例化之后做一些操作。

```java
protected void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) {
    }
```

继续往下走：invokeBeanFactoryPostProcessors

```java
protected void invokeBeanFactoryPostProcessors(ConfigurableListableBeanFactory beanFactory) {
    //这是一个比较复杂的方法了比较长。后面再看这个方法。
    //调用所有的BeanFactoryPostProcessors 
    //getBeanFactoryPostProcessors()这里获取的是一个list的集合。
    PostProcessorRegistrationDelegate.invokeBeanFactoryPostProcessors(beanFactory, getBeanFactoryPostProcessors());
}
```

PostProcessorRegistrationDelegate 包含了beanPostProcessors的注册，和BeanFactoryPostProcessors的调用

```java
public static void invokeBeanFactoryPostProcessors(
        ConfigurableListableBeanFactory beanFactory, List<BeanFactoryPostProcessor> beanFactoryPostProcessors) {

    // Invoke BeanDefinitionRegistryPostProcessors first, if any.
    Set<String> processedBeans = new HashSet<String>();
    // 如果bean
    if (beanFactory instanceof BeanDefinitionRegistry) {
        BeanDefinitionRegistry registry = (BeanDefinitionRegistry) beanFactory;
        List<BeanFactoryPostProcessor> regularPostProcessors = new LinkedList<BeanFactoryPostProcessor>();
        List<BeanDefinitionRegistryPostProcessor> registryPostProcessors =
                new LinkedList<BeanDefinitionRegistryPostProcessor>();

        for (BeanFactoryPostProcessor postProcessor : beanFactoryPostProcessors) {
            //如果是BeanDefinitionRegistryPostProcessor的后置处理器就调用postProcessBeanDefinitionRegistry方法。然后加入registryPostProcessors集合
            if (postProcessor instanceof BeanDefinitionRegistryPostProcessor) {
                BeanDefinitionRegistryPostProcessor registryPostProcessor =
                        (BeanDefinitionRegistryPostProcessor) postProcessor;

                registryPostProcessor.postProcessBeanDefinitionRegistry(registry);
                registryPostProcessors.add(registryPostProcessor);
            }
            else {
                //否则就加入到寻常的后置处理器集合
                regularPostProcessors.add(postProcessor);
            }
        }

        // Do not initialize FactoryBeans here: We need to leave all regular beans
        // uninitialized to let the bean factory post-processors apply to them!
        // Separate between BeanDefinitionRegistryPostProcessors that implement
        // PriorityOrdered, Ordered, and the rest.
        //从容器中获取所有的BeanDefinitionRegistryPostProcessor后置处理器
        String[] postProcessorNames =
                beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);

        // First, invoke the BeanDefinitionRegistryPostProcessors that implement PriorityOrdered.
        // 获取@PriorityOrdered标记的BeanDefinitionRegistryPostProcessors
        List<BeanDefinitionRegistryPostProcessor> priorityOrderedPostProcessors = new ArrayList<BeanDefinitionRegistryPostProcessor>();
        for (String ppName : postProcessorNames) {

            if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
                priorityOrderedPostProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
                processedBeans.add(ppName);
            }
        }
        //排序
        sortPostProcessors(beanFactory, priorityOrderedPostProcessors);
        registryPostProcessors.addAll(priorityOrderedPostProcessors);
        //按照顺序调用BeanDefinitionRegistryPostProcessor
        invokeBeanDefinitionRegistryPostProcessors(priorityOrderedPostProcessors, registry);

        //获取@Order标记的BeanDefinitionRegistryPostProcessor
        postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
        List<BeanDefinitionRegistryPostProcessor> orderedPostProcessors = new ArrayList<BeanDefinitionRegistryPostProcessor>();
        for (String ppName : postProcessorNames) {
            //去除已经@PriorityOrdered标记的类，防止两个注解，同时找到，调用多次
            if (!processedBeans.contains(ppName) && beanFactory.isTypeMatch(ppName, Ordered.class)) {
                orderedPostProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
                processedBeans.add(ppName);
            }
        }
        sortPostProcessors(beanFactory, orderedPostProcessors);
        registryPostProcessors.addAll(orderedPostProcessors);
        //按照顺序调用BeanDefinitionRegistryPostProcessor
        invokeBeanDefinitionRegistryPostProcessors(orderedPostProcessors, registry);

        // 
        boolean reiterate = true;
        while (reiterate) {
            reiterate = false;
            postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
            for (String ppName : postProcessorNames) {
                if (!processedBeans.contains(ppName)) {
                    BeanDefinitionRegistryPostProcessor pp = beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class);
                    registryPostProcessors.add(pp);
                    processedBeans.add(ppName);
                    pp.postProcessBeanDefinitionRegistry(registry);
                    reiterate = true;
                }
            }
        }

        // 调用BeanDefinitionRegistryPostProcessor类的回调方法postProcessBeanFactory()
        invokeBeanFactoryPostProcessors(registryPostProcessors, beanFactory);
        // 寻常bean的回调方法postProcessBeanFactory
        invokeBeanFactoryPostProcessors(regularPostProcessors, beanFactory);
    }

    else {
        // 调用回调方法postProcessBeanFactory()
        invokeBeanFactoryPostProcessors(beanFactoryPostProcessors, beanFactory);
    }


    String[] postProcessorNames =
            beanFactory.getBeanNamesForType(BeanFactoryPostProcessor.class, true, false);

    // Separate between BeanFactoryPostProcessors that implement PriorityOrdered,
    // Ordered, and the rest.
    List<BeanFactoryPostProcessor> priorityOrderedPostProcessors = new ArrayList<BeanFactoryPostProcessor>();
    List<String> orderedPostProcessorNames = new ArrayList<String>();
    List<String> nonOrderedPostProcessorNames = new ArrayList<String>();
    for (String ppName : postProcessorNames) {
        if (processedBeans.contains(ppName)) {
            // skip - already processed in first phase above
        }
        else if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
            priorityOrderedPostProcessors.add(beanFactory.getBean(ppName, BeanFactoryPostProcessor.class));
        }
        else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
            orderedPostProcessorNames.add(ppName);
        }
        else {
            nonOrderedPostProcessorNames.add(ppName);
        }
    }

    sortPostProcessors(beanFactory, priorityOrderedPostProcessors);
    invokeBeanFactoryPostProcessors(priorityOrderedPostProcessors, beanFactory);

    List<BeanFactoryPostProcessor> orderedPostProcessors = new ArrayList<BeanFactoryPostProcessor>();
    for (String postProcessorName : orderedPostProcessorNames) {
        orderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
    }
    sortPostProcessors(beanFactory, orderedPostProcessors);
    invokeBeanFactoryPostProcessors(orderedPostProcessors, beanFactory);

    List<BeanFactoryPostProcessor> nonOrderedPostProcessors = new ArrayList<BeanFactoryPostProcessor>();
    for (String postProcessorName : nonOrderedPostProcessorNames) {
        nonOrderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
    }
    invokeBeanFactoryPostProcessors(nonOrderedPostProcessors, beanFactory);

    // 清理元数据缓存
    beanFactory.clearMetadataCache();
}
```

### registerBeanPostProcessors

继续往下走：registerBeanPostProcessors

```java
protected void registerBeanPostProcessors(ConfigurableListableBeanFactory beanFactory) {
    //注册BeanPostProcessors后面统一看这个PostProcessorRegistrationDelegate
    PostProcessorRegistrationDelegate.registerBeanPostProcessors(beanFactory, this);
}
```

### initMessageSource

继续往下看：initMessageSource 用以支持Spring国际化。

```java
protected void initMessageSource() {
    //拿到当前的beanFactory
    ConfigurableListableBeanFactory beanFactory = getBeanFactory();
    //如果已经存在MessageSource了
    if (beanFactory.containsLocalBean(MESSAGE_SOURCE_BEAN_NAME)) {
        this.messageSource = beanFactory.getBean(MESSAGE_SOURCE_BEAN_NAME, MessageSource.class);
        // Make MessageSource aware of parent MessageSource.
        if (this.parent != null && this.messageSource instanceof HierarchicalMessageSource) {
            //HierarchicalMessageSource采用的职责链的设计模式。
            //如果消息当前对象处理不了，就将消息给上级父对象处理，把消息分层次处理。
            HierarchicalMessageSource hms = (HierarchicalMessageSource) this.messageSource;
            if (hms.getParentMessageSource() == null) {
                //如果父消息源不为空，就设置父消息源，
                hms.setParentMessageSource(getInternalParentMessageSource());
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Using MessageSource [" + this.messageSource + "]");
        }
    }
    else {
        // Use empty MessageSource to be able to accept getMessage calls.
        // 包装一个空的消息源可以用getMessage方法调用。
        DelegatingMessageSource dms = new DelegatingMessageSource();
        // 设置父消息源
        dms.setParentMessageSource(getInternalParentMessageSource());
        this.messageSource = dms;
        beanFactory.registerSingleton(MESSAGE_SOURCE_BEAN_NAME, this.messageSource);
        if (logger.isDebugEnabled()) {
            logger.debug("Unable to locate MessageSource with name '" + MESSAGE_SOURCE_BEAN_NAME +
                    "': using default [" + this.messageSource + "]");
        }
    }
}
```

### initApplicationEventMulticaster

继续：initApplicationEventMulticaster 初始化事件广播器。可以通过multicastEvent方法广播消息

```java
protected void initApplicationEventMulticaster() {
    ConfigurableListableBeanFactory beanFactory = getBeanFactory();
    //如果容器里面有就直接拿出来用，如果没有就初始化一个。
    if (beanFactory.containsLocalBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME)) {
        this.applicationEventMulticaster =
                beanFactory.getBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, ApplicationEventMulticaster.class);
        if (logger.isDebugEnabled()) {
            logger.debug("Using ApplicationEventMulticaster [" + this.applicationEventMulticaster + "]");
        }
    }
    else {
        this.applicationEventMulticaster = new SimpleApplicationEventMulticaster(beanFactory);
        beanFactory.registerSingleton(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, this.applicationEventMulticaster);
        if (logger.isDebugEnabled()) {
            logger.debug("Unable to locate ApplicationEventMulticaster with name '" +
                    APPLICATION_EVENT_MULTICASTER_BEAN_NAME +
                    "': using default [" + this.applicationEventMulticaster + "]");
        }
    }
}
```

### onRefresh

继续：onRefresh方法也是一个模版方法，空方法，目的也是为了给子类继承用的。AbstractRefreshableWebApplicationContext、StaticWebApplicationContext用这个方法来刷新初始化主题源。  
继续：registerListeners 注册监听器

```java
protected void registerListeners() {
    //把监听者加入到事件广播器
    for (ApplicationListener<?> listener : getApplicationListeners()) {
        getApplicationEventMulticaster().addApplicationListener(listener);
    }

    //获取所有的ApplicationListener的bean的名字，然后把bean名字加入到事件广播器
    String[] listenerBeanNames = getBeanNamesForType(ApplicationListener.class, true, false);
    for (String listenerBeanName : listenerBeanNames) {
        getApplicationEventMulticaster().addApplicationListenerBean(listenerBeanName);
    }

    //拿到所有的earlyApplicationEvents事件消息，直接广播发送事件给所有的监听者。
    Set<ApplicationEvent> earlyEventsToProcess = this.earlyApplicationEvents;
    this.earlyApplicationEvents = null;
    if (earlyEventsToProcess != null) {
        for (ApplicationEvent earlyEvent : earlyEventsToProcess) {
            getApplicationEventMulticaster().multicastEvent(earlyEvent);
        }
    }
}
```

### finishBeanFactoryInitialization

继续：finishBeanFactoryInitialization  
初始化非延迟加载的单例Bean， 实例化BeanFactory中已经被注册但是未实例化的所有实例\(@Lazy注解的Bean不在此实例化\)。

```java
protected void finishBeanFactoryInitialization(ConfigurableListableBeanFactory beanFactory) {
    // 初始化类型转换器
    if (beanFactory.containsBean(CONVERSION_SERVICE_BEAN_NAME) &&
            beanFactory.isTypeMatch(CONVERSION_SERVICE_BEAN_NAME, ConversionService.class)) {
        beanFactory.setConversionService(
                beanFactory.getBean(CONVERSION_SERVICE_BEAN_NAME, ConversionService.class));
    }

    //获取LoadTimeWeaverAware.class的单例bean
    String[] weaverAwareNames = beanFactory.getBeanNamesForType(LoadTimeWeaverAware.class, false, false);
    for (String weaverAwareName : weaverAwareNames) {
        getBean(weaverAwareName);
    }

    // 停止使用零时加载器
    beanFactory.setTempClassLoader(null);

    // 允许缓存所有的bean的定义，不允许修改
    beanFactory.freezeConfiguration();

    // 初始化所有的单例bean，@lazy bean不在这里初始化
    beanFactory.preInstantiateSingletons();
}
```

### finishRefresh

继续：finishRefresh   
refresh结束之前需要做善后工作。包括生命周期组件LifecycleProcessor的初始化和调用、事件发布、JMX组件的处理等。

```java
protected void finishRefresh() {
    // 初始化生命周期组件LifecycleProcessor
    initLifecycleProcessor();

    // 调用一次生命周期组件LifecycleProcessor
    getLifecycleProcessor().onRefresh();

    // 发布容器刷新事件
    publishEvent(new ContextRefreshedEvent(this));

    // 向MBeanServer注册LiveBeansView，可以通过JMX来监控此ApplicationContext。
    LiveBeansView.registerApplicationContext(this);
}
```
这个类refresh方法干的活也是有很多，其中就包括BeanFactory的设置、Configuration类解析、Bean实例化、属性和依赖注入、事件监听器注册。下面会继续去分析一下每一步是怎样实现的。

# Environment 接口

## 继承图谱

![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1558102717211.png)

往上继承了PropertyResolver 属性解析器，Environment接口里面有三个独立的方法如下：

```java
String[] getDefaultProfiles();
String[] getActiveProfiles();
boolean acceptsProfiles(String... profiles);
```
都和Profile有关系。Spring Profile特性是从3.1开始的，其主要是为了解决这样一种问题: 线上环境和测试环境使用不同的配置或是数据库或是其它。有了Profile便可以在 不同环境之间无缝切换。**Spring容器管理的所有bean都是和一个profile绑定在一起的。**使用了Profile的配置文件示例:
```xml
<beans profile="develop">  
    <context:property-placeholder location="classpath*:jdbc-develop.properties"/>  
</beans>  
<beans profile="production">  
    <context:property-placeholder location="classpath*:jdbc-production.properties"/>  
</beans>  
<beans profile="test">  
    <context:property-placeholder location="classpath*:jdbc-test.properties"/>  
</beans>
```
可以通过context.getEnvironment().setActiveProfiles("dev");
或者spring.profiles.active=dev 进行设置。spring 中Environment 默认就是 StandardEnvironment实例。
```java
public class StandardEnvironment extends AbstractEnvironment {

	/** System environment property source name: {@value} */
	//系统级环境参数可以通过{@systemEnvironment[xxx]},或者{#systemEnvironment[xxx]}获取
	public static final String SYSTEM_ENVIRONMENT_PROPERTY_SOURCE_NAME = "systemEnvironment";

	/** JVM system properties property source name: {@value} */
	//jvm层面级参数可以通过{@systemProperties[xxx]},或者{#systemProperties[xxx]}获取
	public static final String SYSTEM_PROPERTIES_PROPERTY_SOURCE_NAME = "systemProperties";
	//在实例化的时候，会调用父类里面的构造方法，而父类的构造方法里会调用此方法。
	protected void customizePropertySources(MutablePropertySources propertySources) {
		//MapPropertySource其实就是MaP对象的封装
		propertySources.addLast(new MapPropertySource(SYSTEM_PROPERTIES_PROPERTY_SOURCE_NAME, getSystemProperties()));
		//SystemEnvironmentPropertySource继承的MapPropertySource，其实里面也是map对象
		propertySources.addLast(new SystemEnvironmentPropertySource(SYSTEM_ENVIRONMENT_PROPERTY_SOURCE_NAME, getSystemEnvironment()));
	}

}
```
MutablePropertySources是PropertySources的实现类。里面封装了一个Log对象，和用一个CopyOnWriteArrayList实现的一个PropertySource的一个集合，里面有一个PropertySourcesPropertyResolver解析器，这个解析器在PropertyResolver章节分析。
在StandardEnvironment实例化的时调用AbstractEnvironment构造方法。
```java
public AbstractEnvironment() {
	//此时这里就会被子类的customizePropertySources给复写掉。会调用子类的方法。
	//此时的this.propertySources=new MutablePropertySources(this.logger);
	//此时MutablePropertySources对象只有龙对象，PropertySource集合是空的
	//通过子类的propertySources.addLast往里面加入PropertySource对象。

	customizePropertySources(this.propertySources);
	if (this.logger.isDebugEnabled()) {
		this.logger.debug(format(
				"Initialized %s with PropertySources %s", getClass().getSimpleName(), this.propertySources));
	}
}
```
再看看StandardEnvironment#getSystemProperties函数:
这个函数就是调用System.getProperties获取所有的系统配置，如果系统管理说没有权限获取，就一条一条的获取，这个地方我不甚理解。why？
```
public Map<String, Object> getSystemProperties() {
	try {
		return (Map) System.getProperties();
	}
	catch (AccessControlException ex) {
		return (Map) new ReadOnlySystemAttributesMap() {
			@Override
			protected String getSystemAttribute(String attributeName) {
				try {
					return System.getProperty(attributeName);
				}
				catch (AccessControlException ex) {
					if (logger.isInfoEnabled()) {
						logger.info(format("Caught AccessControlException when accessing system " +
								"property [%s]; its value will be returned [null]. Reason: %s",
								attributeName, ex.getMessage()));
					}
					return null;
				}
			}
		};
	}
}
```
同样的getSystemEnvironment函数：
是调用的System.getenv获取jvm级系统参数，包活jdk版本，os参数等。
```java
public Map<String, Object> getSystemEnvironment() {
	//这一句会从spring.properties文件里找spring.getenv.ignore标识
	//如果spring.getenv.ignore=true就返回空，
	//如果不为空就调用System.getenv获取jvm系统级参数。
	if (suppressGetenvAccess()) {
		return Collections.emptyMap();
	}
	try {
		return (Map) System.getenv();
	}
	catch (AccessControlException ex) {
		return (Map) new ReadOnlySystemAttributesMap() {
			@Override
			protected String getSystemAttribute(String attributeName) {
				try {
					return System.getenv(attributeName);
				}
				catch (AccessControlException ex) {
					if (logger.isInfoEnabled()) {
						logger.info(format("Caught AccessControlException when accessing system " +
								"environment variable [%s]; its value will be returned [null]. Reason: %s",
								attributeName, ex.getMessage()));
					}
					return null;
				}
			}
		};
	}
}
```
再看看Environment接口里的三个私有方法的实现：
```java
@Override
public String[] getActiveProfiles() {
	return StringUtils.toStringArray(doGetActiveProfiles());
}
@Override
public String[] getDefaultProfiles() {
	return StringUtils.toStringArray(doGetDefaultProfiles());
}
public boolean acceptsProfiles(String... profiles) {
	Assert.notEmpty(profiles, "Must specify at least one profile");
	for (String profile : profiles) {
		//这里判断的是以！开头的profile配置。
		if (StringUtils.hasLength(profile) && profile.charAt(0) == '!') {
			//双重否定
			if (!isProfileActive(profile.substring(1))) {
				return true;
			}
		}
		else if (isProfileActive(profile)) {
			return true;
		}
	}
	return false;
}
```
```java
protected Set<String> doGetActiveProfiles() {
	synchronized (this.activeProfiles) {
		if (this.activeProfiles.isEmpty()) {
			//拿到spring.profiles.active的配置。
			//spring.profiles.active=dev,prod 如果有多个可以用逗号分割。实际应用估计也很少用到多个吧。
			String profiles = getProperty(ACTIVE_PROFILES_PROPERTY_NAME);
			if (StringUtils.hasText(profiles)) {
				//拿到值去除空白字符串按照逗号分割成一个数组
				setActiveProfiles(commaDelimitedListToStringArray(trimAllWhitespace(profiles)));
			}
		}
		return this.activeProfiles;
	}
}
```
```java
protected Set<String> doGetDefaultProfiles() {
	synchronized (this.defaultProfiles) {
	//如果是default就拿到spring.profiles.default的配置的值，
	//同样spring.profiles.default也是可以配置多个的，按照逗号分隔。
		if (this.defaultProfiles.equals(getReservedDefaultProfiles())) {
			String profiles = getProperty(DEFAULT_PROFILES_PROPERTY_NAME);
			if (StringUtils.hasText(profiles)) {
				setDefaultProfiles(commaDelimitedListToStringArray(trimAllWhitespace(profiles)));
			}
		}
		return this.defaultProfiles;
	}
}
```
以上关于环境配置相关配置的代码阅读。

# PropertyResolver接口

## 继承图谱 

![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1558145690624.png)

PropertySourcesPropertyResolver类里面有一个PropertySources的成员变量。类的很多方法实现都是调用的这个PropertySources成员变量的方法。PropertySourcesPropertyResolver可以通过getProperty(key)的方式获取对应的value值。
那么PropertySourcesPropertyResolver有哪些东西呢？主要看这个方法：
```java
protected <T> T getProperty(String key, Class<T> targetValueType, boolean resolveNestedPlaceholders) {
	boolean debugEnabled = logger.isDebugEnabled();
	if (logger.isTraceEnabled()) {
		logger.trace(String.format("getProperty(\"%s\", %s)", key, targetValueType.getSimpleName()));
	}
	if (this.propertySources != null) {
		//遍历所有已经加载到的PropertySource
		for (PropertySource<?> propertySource : this.propertySources) {
			if (debugEnabled) {
				logger.debug(String.format("Searching for key '%s' in [%s]", key, propertySource.getName()));
			}
			Object value;
			if ((value = propertySource.getProperty(key)) != null) {
				Class<?> valueType = value.getClass();
				//如果是字符串，同时要求字符替换的就调用字符替换方法

				if (resolveNestedPlaceholders && value instanceof String) {
					value = resolveNestedPlaceholders((String) value);
				}
				if (debugEnabled) {
					logger.debug(String.format("Found key '%s' in [%s] with type [%s] and value '%s'",
							key, propertySource.getName(), valueType.getSimpleName(), value));
				}
				//如果不是字符串类型，就根据属性转换器尽心数据转换，
				//如果类型是属性转换器无法转换的就知道抛出异常
				if (!this.conversionService.canConvert(valueType, targetValueType)) {
					throw new IllegalArgumentException(String.format(
							"Cannot convert value [%s] from source type [%s] to target type [%s]",
							value, valueType.getSimpleName(), targetValueType.getSimpleName()));
				}
				return this.conversionService.convert(value, targetValueType);
			}
		}
	}
	if (debugEnabled) {
		logger.debug(String.format("Could not find key '%s' in any property source. Returning [null]", key));
	}
	return null;
}
```

看看resolveNestedPlaceholders方法
```java
protected String resolveNestedPlaceholders(String value) {
	//如果PropertySourcesPropertyResolver上属性设置了ignoreUnresolvableNestedPlaceholders值为true可以忽略一些不存在key的属性。
	//如果为false，key不存在的属性直接就会抛出异常。
	return (this.ignoreUnresolvableNestedPlaceholders ?
			resolvePlaceholders(value) : resolveRequiredPlaceholders(value));
}
```
```java
public String resolveRequiredPlaceholders(String text) throws IllegalArgumentException {
	if (this.strictHelper == null) {
		//实例化一个PropertyPlaceholderHelper类，
		this.strictHelper = createPlaceholderHelper(false);
	}
	//用PropertyPlaceholderHelper类去解析属性
	return doResolvePlaceholders(text, this.strictHelper);
}
```
```java
@Override
public String resolvePlaceholders(String text) {
	if (this.nonStrictHelper == null) {
		this.nonStrictHelper = createPlaceholderHelper(true);
	}
	return doResolvePlaceholders(text, this.nonStrictHelper);
}
```
createPlaceholderHelper：
```java
//根据ignoreUnresolvablePlaceholders来创建PropertyPlaceholderHelper
//placeholderPrefix 是替换的前缀，默认值是${
//placeholderSuffix 是替换的后缀，默认值是}
//valueSeparator 是值的分隔符，默认是：
private PropertyPlaceholderHelper createPlaceholderHelper(boolean ignoreUnresolvablePlaceholders) {
	return new PropertyPlaceholderHelper(this.placeholderPrefix, this.placeholderSuffix,
			this.valueSeparator, ignoreUnresolvablePlaceholders);
}
```
看看PropertyPlaceholderHelper：




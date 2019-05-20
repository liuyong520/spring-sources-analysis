# configuration的加载

```java
@Override
protected void loadBeanDefinitions(DefaultListableBeanFactory beanFactory) throws BeansException, IOException {
    //创建一个读取bean的配置文件的加载器
    XmlBeanDefinitionReader beanDefinitionReader = new XmlBeanDefinitionReader(beanFactory);

    // Configure the bean definition reader with this context's
    // resource loading environment.
    // 父类被设值了就是StandardEnvironment
    beanDefinitionReader.setEnvironment(this.getEnvironment());
    // 这里其实被赋值的是DefaultResourceLoader的子类。
    beanDefinitionReader.setResourceLoader(this);
    // 设置资源实体的解析器
    beanDefinitionReader.setEntityResolver(new ResourceEntityResolver(this));

    // Allow a subclass to provide custom initialization of the reader,
    // then proceed with actually loading the bean definitions.
    initBeanDefinitionReader(beanDefinitionReader);
    loadBeanDefinitions(beanDefinitionReader);
}
```
## XmlBeanDefinitionReader
先看一下这个类的继承图谱
### 继承图谱
![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1558273300511.png)
### 构造方法
XmlBeanDefinitionReader会调用父类的构造方法尽行初始化环境，初始化类加载器。
```
protected AbstractBeanDefinitionReader(BeanDefinitionRegistry registry) {
    Assert.notNull(registry, "BeanDefinitionRegistry must not be null");
    this.registry = registry;

    //DefaultListableBeanFactory不是ResourceLoader的类型
    if (this.registry instanceof ResourceLoader) {
        this.resourceLoader = (ResourceLoader) this.registry;
    }
    else {
        //资源加载器PathMatchingResourcePatternResolver
        //但是会被子类的setResourceLoader覆盖掉。
        this.resourceLoader = new PathMatchingResourcePatternResolver();
    }

    //DefaultListableBeanFactory也不是EnvironmentCapable
    if (this.registry instanceof EnvironmentCapable) {
        this.environment = ((EnvironmentCapable) this.registry).getEnvironment();
    }
    else {
        //初始化环境变量
        this.environment = new StandardEnvironment();
    }
}
```

### bean的加载

```java
public int loadBeanDefinitions(String location, Set<Resource> actualResources) throws BeanDefinitionStoreException {
    ResourceLoader resourceLoader = getResourceLoader();
    if (resourceLoader == null) {
        throw new BeanDefinitionStoreException(
                "Cannot import bean definitions from location [" + location + "]: no ResourceLoader available");
    }
    //因为ClassPathApplicationContext实现了ResourcePatternResolver 

    if (resourceLoader instanceof ResourcePatternResolver) {
        // Resource pattern matching available.
        try {
            //这一句会拿到ResourcePatternResolver的对象。
            //加载资源文件
            Resource[] resources = ((ResourcePatternResolver) resourceLoader).getResources(location);
            //加载所有的bean
            int loadCount = loadBeanDefinitions(resources);
            //这里不会执行，因为actualResources是null
            if (actualResources != null) {
                for (Resource resource : resources) {
                    actualResources.add(resource);
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("Loaded " + loadCount + " bean definitions from location pattern [" + location + "]");
            }
            return loadCount;
        }
        catch (IOException ex) {
            throw new BeanDefinitionStoreException(
                    "Could not resolve bean definition resource pattern [" + location + "]", ex);
        }
    }
    else {
        // Can only load single resources by absolute URL.
        Resource resource = resourceLoader.getResource(location);
        int loadCount = loadBeanDefinitions(resource);
        if (actualResources != null) {
            actualResources.add(resource);
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Loaded " + loadCount + " bean definitions from location [" + location + "]");
        }
        return loadCount;
    }
}
    
```
### getResources

看看getResources的方法
```java
public Resource[] getResources(String locationPattern) throws IOException {
    // 因为resourcePatternResolver是PathMatchingResourcePatternResolver的实例
    // 所以会调用PathMatchingResourcePatternResolver的getResources方法
    return this.resourcePatternResolver.getResources(locationPattern);
}
```

```java

public Resource[] getResources(String locationPattern) throws IOException {
    Assert.notNull(locationPattern, "Location pattern must not be null");
    //如果是以classpath*:开头的
    if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) {
        // a class path resource (multiple resources for same name possible)
        // 拿到的是AntPathMatcher实例。
        // 如果包含*或者？就匹配成功
        if (getPathMatcher().isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) {
            // a class path resource pattern
            //
            return findPathMatchingResources(locationPattern);
        }
        else {
            // all class path resources with the given name
            // 路径没有？或者*
            return findAllClassPathResources(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()));
        }
    }
    else {
        // Only look for a pattern after a prefix here
        // (to not get fooled by a pattern symbol in a strange prefix).
        int prefixEnd = locationPattern.indexOf(":") + 1;
        if (getPathMatcher().isPattern(locationPattern.substring(prefixEnd))) {
            // a file pattern
            return findPathMatchingResources(locationPattern);
        }
        else {
            // a single resource with the given name
            return new Resource[] {getResourceLoader().getResource(locationPattern)};
        }
    }
}
```
看看里面
```java
public boolean isPattern(String path) {
    return (path.indexOf('*') != -1 || path.indexOf('?') != -1);
}
```
### findPathMatchingResources
```java
protected Resource[] findPathMatchingResources(String locationPattern) throws IOException {
    //如果是"/WEB-INF/*.xml 拿到的值是"/WEB-INF/"
    //如果是"/WEB-INF/*/*.xml"拿到是“/WEB-INF/*/”
    //如果是“classpath：context/*.xml”拿到的是“classpath：context/”
    // 获取文件的根路径
    String rootDirPath = determineRootDir(locationPattern);
    // 获取正则表达式
    String subPattern = locationPattern.substring(rootDirPath.length());
    // 重新调用getResources，两个方法又开始循环调用了
    // 如果是“classpath：context/”那此事会调用getResources里的子方法findAllClassPathResources
    // findAllClassPathResources会拿到目录下的所有资源
    Resource[] rootDirResources = getResources(rootDirPath);
    Set<Resource> result = new LinkedHashSet<Resource>(16);
    for (Resource rootDirResource : rootDirResources) {
        
        rootDirResource = resolveRootDirResource(rootDirResource);
        //加载vfs文件
        if (rootDirResource.getURL().getProtocol().startsWith(ResourceUtils.URL_PROTOCOL_VFS)) {
            result.addAll(VfsResourceMatchingDelegate.findMatchingResources(rootDirResource, subPattern, getPathMatcher()));
        }
        //加载jar里面的文件
        else if (isJarResource(rootDirResource)) {
            result.addAll(doFindPathMatchingJarResources(rootDirResource, subPattern));
        }
        else {
            //最后才是加载本地系统的文件
            result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern));
        }
    }
    if (logger.isDebugEnabled()) {
        logger.debug("Resolved location pattern [" + locationPattern + "] to resources " + result);
    }
    return result.toArray(new Resource[result.size()]);
}
```
### findAllClassPathResources

```java
protected Resource[] findAllClassPathResources(String location) throws IOException {
    String path = location;
    if (path.startsWith("/")) {
        path = path.substring(1);
    }
    Set<Resource> result = doFindAllClassPathResources(path);
    return result.toArray(new Resource[result.size()]);
}
```
再看看
```java
protected Set<Resource> doFindAllClassPathResources(String path) throws IOException {
    Set<Resource> result = new LinkedHashSet<Resource>(16);
    ClassLoader cl = getClassLoader();
    Enumeration<URL> resourceUrls = (cl != null ? cl.getResources(path) : ClassLoader.getSystemResources(path));
    while (resourceUrls.hasMoreElements()) {
        URL url = resourceUrls.nextElement();
        result.add(convertClassLoaderURL(url));
    }
    if ("".equals(path)) {
        // The above result is likely to be incomplete, i.e. only containing file system references.
        // We need to have pointers to each of the jar files on the classpath as well...
        addAllClassLoaderJarRoots(cl, result);
    }
    return result;
}
```
说到这里仅仅也只是spring是如何找文件的。这里还没有文件的读取和解析。
下面介绍spring配置文件的读取和解析。
```java 
public int loadBeanDefinitions(EncodedResource encodedResource) throws BeanDefinitionStoreException {
    Assert.notNull(encodedResource, "EncodedResource must not be null");
    if (logger.isInfoEnabled()) {
        logger.info("Loading XML bean definitions from " + encodedResource.getResource());
    }
    // TheadLocal的已经加载的资源set集合。
    Set<EncodedResource> currentResources = this.resourcesCurrentlyBeingLoaded.get();
    if (currentResources == null) {
        currentResources = new HashSet<EncodedResource>(4);
        this.resourcesCurrentlyBeingLoaded.set(currentResources);
    }
    if (!currentResources.add(encodedResource)) {
        throw new BeanDefinitionStoreException(
                "Detected cyclic loading of " + encodedResource + " - check your import definitions!");
    }
    //以下这段代码就是真实读取文件的逻辑了。
    try {
        InputStream inputStream = encodedResource.getResource().getInputStream();
        try {
            InputSource inputSource = new InputSource(inputStream);
            if (encodedResource.getEncoding() != null) {
                inputSource.setEncoding(encodedResource.getEncoding());
            }
            return doLoadBeanDefinitions(inputSource, encodedResource.getResource());
        }
        finally {
            inputStream.close();
        }
    }
    catch (IOException ex) {
        throw new BeanDefinitionStoreException(
                "IOException parsing XML document from " + encodedResource.getResource(), ex);
    }
    finally {
        currentResources.remove(encodedResource);
        if (currentResources.isEmpty()) {
            this.resourcesCurrentlyBeingLoaded.remove();
        }
    }
}
```
### doLoadBeanDefinitions
```java
protected int doLoadBeanDefinitions(InputSource inputSource, Resource resource)
            throws BeanDefinitionStoreException {
    // 读取文件
    Document doc = doLoadDocument(inputSource, resource);
    // 这个方法里面就是配置文件的解析了。
    return registerBeanDefinitions(doc, resource);
}
```
### doLoadDocument
```java
protected Document doLoadDocument(InputSource inputSource, Resource resource) throws Exception {
    // documentLoader是一个DefaultDocumentLoader对象，此类是DocumentLoader接口的唯一实现。
    // getEntityResolver方法返回ResourceEntityResolver,
    // ResourceEntityResolver会用xsd或者dtd约束文件做校验。
    // errorHandler是一个SimpleSaxErrorHandler对象。
    return this.documentLoader.loadDocument(inputSource, getEntityResolver(), this.errorHandler,
            getValidationModeForResource(resource), isNamespaceAware());
}
```

### loadDocument

```java
/**
** 这里就是老套路了，可以看出，Spring还是使用了dom的方式解析，即一次全部load到内存
**/
public Document loadDocument(InputSource inputSource, EntityResolver entityResolver,
        ErrorHandler errorHandler, int validationMode, boolean namespaceAware) throws Exception {

    DocumentBuilderFactory factory = createDocumentBuilderFactory(validationMode, namespaceAware);
    if (logger.isDebugEnabled()) {
        logger.debug("Using JAXP provider [" + factory.getClass().getName() + "]");
    }
    DocumentBuilder builder = createDocumentBuilder(factory, entityResolver, errorHandler);
    return builder.parse(inputSource);
}
```
```java
protected DocumentBuilderFactory createDocumentBuilderFactory(int validationMode, boolean namespaceAware{
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(namespaceAware);
    if (validationMode != XmlValidationModeDetector.VALIDATION_NONE) {
        //此方法设为true仅对dtd有效，xsd(schema)无效
        factory.setValidating(true);
        if (validationMode == XmlValidationModeDetector.VALIDATION_XSD) {
            // Enforce namespace aware for XSD...
             //开启xsd(schema)支持
            factory.setNamespaceAware(true);
             //这个也是Java支持Schema的套路，可以问度娘
            factory.setAttribute(SCHEMA_LANGUAGE_ATTRIBUTE, XSD_SCHEMA_LANGUAGE);
        }
    }
    return factory;
}
```
## bean解析
### registerBeanDefinitions
瞧一下这个方法，看看做了哪些事情。
```java
public int registerBeanDefinitions(Document doc, Resource resource) throws BeanDefinitionStoreException {
    //根据反射的方式创建DefaultBeanDefinitionDocumentReader对象。
    //这其实也是策略模式，通过setter方法可以更换其实现。修改documentReaderClass参数即可
    BeanDefinitionDocumentReader documentReader = createBeanDefinitionDocumentReader();
    //获取bean定义的数量
    int countBefore = getRegistry().getBeanDefinitionCount();
    //读取文件
    documentReader.registerBeanDefinitions(doc, createReaderContext(resource));
    return getRegistry().getBeanDefinitionCount() - countBefore;
}
```

### createReaderContext

```java
public XmlReaderContext createReaderContext(Resource resource) {
    // problemReporter是一个FailFastProblemReporter对象。
    // eventListener是EmptyReaderEventListener对象，此类里的方法都是空实现。
    // sourceExtractor是NullSourceExtractor对象，直接返回空，也是空实现。
    // getNamespaceHandlerResolver默认返回DefaultNamespaceHandlerResolver对象，用来获取xsd对应的处理器。

    return new XmlReaderContext(resource, this.problemReporter, this.eventListener,
        this.sourceExtractor, this, getNamespaceHandlerResolver());
}
```
XmlReaderContext的作用感觉就是这一堆参数的容器，糅合到一起传给DocumentReader，并美其名为Context。可以看出，Spring中到处都是策略模式，大量操作被抽象成接口。

### registerBeanDefinitions
此方式是在DefaultBeanDefinitionDocumentReader的里面实现的。
```java
@Override
public void registerBeanDefinitions(Document doc, XmlReaderContext readerContext) {
    this.readerContext = readerContext;
    //获取根节点beans
    Element root = doc.getDocumentElement();
    //注册根节点下所有的bean
    doRegisterBeanDefinitions(root);
}
``` 
### doRegisterBeanDefinitions

```java
protected void doRegisterBeanDefinitions
(Element root) {
        // Any nested <beans> elements will cause recursion in this method. In
        // order to propagate and preserve <beans> default-* attributes correctly,
        // keep track of the current (parent) delegate, which may be null. Create
        // the new (child) delegate with a reference to the parent for fallback purposes,
        // then ultimately reset this.delegate back to its original (parent) reference.
        // this behavior emulates a stack of delegates without actually necessitating one.
        BeanDefinitionParserDelegate parent = this.delegate;
        this.delegate = createDelegate(getReaderContext(), root, parent);
        // 默认的命名空间即
        // http://www.springframework.org/schema/beans
        if (this.delegate.isDefaultNamespace(root)) {
            // 检查profile属性,获取profile属性
            String profileSpec = root.getAttribute(PROFILE_ATTRIBUTE);
            if (StringUtils.hasText(profileSpec)) {
                // 分隔profile属性的值 ,分割
                String[] specifiedProfiles = StringUtils.tokenizeToStringArray(
                        profileSpec, BeanDefinitionParserDelegate.MULTI_VALUE_ATTRIBUTE_DELIMITERS);
                // 如果不是可用的profile的值，就直接返回
                if (!getReaderContext().getEnvironment().acceptsProfiles(specifiedProfiles)) {
                    return;
                }
            }
        }
        // 预处理xml方法这是个空方法，
        // 我们可以扩展这个方法，来加载解析我们自己的自定义标签。
        preProcessXml(root);
        // 解析
        parseBeanDefinitions(root, this.delegate);
        postProcessXml(root);

        this.delegate = parent;
    }
```
### parseBeanDefinitions

```java
`protected void parseBeanDefinitions(Element root, BeanDefinitionParserDelegate delegate) {
    // 验证名称空间
    // http://www.springframework.org/schema/beans
    if (delegate.isDefaultNamespace(root)) {

        NodeList nl = root.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node node = nl.item(i);
            if (node instanceof Element) {
                Element ele = (Element) node;
                // 检查节点是不是
                // http://www.springframework.org/schema/beans
                if (delegate.isDefaultNamespace(ele)) {
                    // 默认解析方式xml
                    parseDefaultElement(ele, delegate);
                }
                else {
                    //自定义解析xml
                    delegate.parseCustomElement(ele);
                }
            }
        }
    }
    else {
        //自定义解析xml
        delegate.parseCustomElement(root);
    }
    }
```

### parseDefaultElement

```java
private void parseDefaultElement(Element ele, BeanDefinitionParserDelegate delegate) {
    // 处理 import标签
    if (delegate.nodeNameEquals(ele, IMPORT_ELEMENT)) {
        importBeanDefinitionResource(ele);
    }
    // 处理 alais 标签
    else if (delegate.nodeNameEquals(ele, ALIAS_ELEMENT)) {
        processAliasRegistration(ele);
    }
    // 处理 bean标签
    else if (delegate.nodeNameEquals(ele, BEAN_ELEMENT)) {
        processBeanDefinition(ele, delegate);
    }
    // 处理beans标签
    // 返回去调用doRegisterBeanDefinitions的方法
    else if (delegate.nodeNameEquals(ele, NESTED_BEANS_ELEMENT)) {
        // recurse
        // 循环调用doRegisterBeanDefinitions
        doRegisterBeanDefinitions(ele);
    }
}
```
### importBeanDefinitionResource
处理import
```java
protected void importBeanDefinitionResource(Element ele) {
    // 获取resource标记的路径
    // <import resource="context:spring.xml"/>
    String location = ele.getAttribute(RESOURCE_ATTRIBUTE);
    if (!StringUtils.hasText(location)) {
        getReaderContext().error("Resource location must not be empty", ele);
        return;
    }

    // Resolve system properties: e.g. "${user.dir}"
    // 字符替换标签
    location = getReaderContext().getEnvironment().resolveRequiredPlaceholders(location);

    Set<Resource> actualResources = new LinkedHashSet<Resource>(4);

    // Discover whether the location is an absolute or relative URI
    boolean absoluteLocation = false;
    try {
        absoluteLocation = ResourcePatternUtils.isUrl(location) || ResourceUtils.toURI(location).isAbsolute();
    }
    catch (URISyntaxException ex) {
        // cannot convert to an URI, considering the location relative
        // unless it is the well-known Spring prefix "classpath*:"
    }

    // Absolute or relative?
    if (absoluteLocation) {
        try {
            int importCount = getReaderContext().getReader().loadBeanDefinitions(location, actualResources);
            if (logger.isDebugEnabled()) {
                logger.debug("Imported " + importCount + " bean definitions from URL location [" + location + "]");
            }
        }
        catch (BeanDefinitionStoreException ex) {
            getReaderContext().error(
                    "Failed to import bean definitions from URL location [" + location + "]", ele, ex);
        }
    }
    else {
        // No URL -> considering resource location as relative to the current file.
        try {
            int importCount;
            Resource relativeResource = getReaderContext().getResource().createRelative(location);
            if (relativeResource.exists()) {
                importCount = getReaderContext().getReader().loadBeanDefinitions(relativeResource);
                actualResources.add(relativeResource);
            }
            else {
                String baseLocation = getReaderContext().getResource().getURL().toString();
                importCount = getReaderContext().getReader().loadBeanDefinitions(
                        StringUtils.applyRelativePath(baseLocation, location), actualResources);
            }
            if (logger.isDebugEnabled()) {
                logger.debug("Imported " + importCount + " bean definitions from relative location [" + location + "]");
            }
        }
        catch (IOException ex) {
            getReaderContext().error("Failed to resolve current resource location", ele, ex);
        }
        catch (BeanDefinitionStoreException ex) {
            getReaderContext().error("Failed to import bean definitions from relative location [" + location + "]",
                    ele, ex);
        }
    }
    Resource[] actResArray = actualResources.toArray(new Resource[actualResources.size()]);
    getReaderContext().fireImportProcessed(location, actResArray, extractSource(ele));
}
```
importBeanDefinitionResource套路和之前的配置文件加载完全一样，不过注意被import进来的文件是先于当前文件被解析的。上面有些周边的代码就不介绍了。

### processAliasRegistration
处理别名
```java
protected void processAliasRegistration(Element ele) {
    // 拿到名字，和别名
    String name = ele.getAttribute(NAME_ATTRIBUTE);
    String alias = ele.getAttribute(ALIAS_ATTRIBUTE);
    boolean valid = true;
    if (!StringUtils.hasText(name)) {
        getReaderContext().error("Name must not be empty", ele);
        valid = false;
    }
    if (!StringUtils.hasText(alias)) {
        getReaderContext().error("Alias must not be empty", ele);
        valid = false;
    }
    if (valid) {
        try {
            // 核心方法，就是在DefaultListableBeanFactor注册别名，
            // 其实就是在一个map里面写入名字和别名的映射关系。
            getReaderContext().getRegistry().registerAlias(name, alias);
        }
        catch (Exception ex) {
            getReaderContext().error("Failed to register alias '" + alias +
                    "' for bean with name '" + name + "'", ele, ex);
        }
        // 触发监听器
        getReaderContext().fireAliasRegistered(name, alias, extractSource(ele));
    }
}
```
其实这个方法就是给一个bean取一个别名：比如有一个bean名为beanA，但是另一个组件想以beanB的名字使用，就可以这样定义:
<alias name="beanA" alias="beanB"/>

### registerAlias
```java
// 其实就是在map里加上一条映射关系。
public void registerAlias(String name, String alias) {
    Assert.hasText(name, "'name' must not be empty");
    Assert.hasText(alias, "'alias' must not be empty");
    if (alias.equals(name)) {
        this.aliasMap.remove(alias);
    }
    else {
        String registeredName = this.aliasMap.get(alias);
        if (registeredName != null) {
            if (registeredName.equals(name)) {
                // An existing alias - no need to re-register
                return;
            }
            if (!allowAliasOverriding()) {
                throw new IllegalStateException("Cannot register alias '" + alias + "' for name '" +
                        name + "': It is already registered for name '" + registeredName + "'.");
            }
        }
        checkForAliasCircle(name, alias);
        this.aliasMap.put(alias, name);
    }
}
```

### processBeanDefinition

处理bean 
```java
protected void processBeanDefinition(Element ele, BeanDefinitionParserDelegate delegate) {
    BeanDefinitionHolder bdHolder = delegate.parseBeanDefinitionElement(ele);
    if (bdHolder != null) {
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
最后会调用BeanDefinitionParserDelegate.parseBeanDefinitionElement
首先获取到id和name属性，name属性支持配置多个，以逗号分隔，如果没有指定id，那么将以第一个name属性值代替。id必须是唯一的，name属性其实是alias的角色，可以和其它的bean重复，如果name也没有配置，那么其实什么也没做。
```java

public BeanDefinitionHolder parseBeanDefinitionElement(Element ele, BeanDefinition containingBean) {
    // 获取ID
    String id = ele.getAttribute(ID_ATTRIBUTE);
    // 获取name
    String nameAttr = ele.getAttribute(NAME_ATTRIBUTE);
    // name属性可以配置多个，用逗号分隔。
    List<String> aliases = new ArrayList<String>();
    if (StringUtils.hasLength(nameAttr)) {
        String[] nameArr = StringUtils.tokenizeToStringArray(nameAttr, MULTI_VALUE_ATTRIBUTE_DELIMITERS);
        aliases.addAll(Arrays.asList(nameArr));
    }

    String beanName = id;
    // 如果id没有配置 就用name的数组的第一个名字代替
    if (!StringUtils.hasText(beanName) && !aliases.isEmpty()) {
        beanName = aliases.remove(0);
        if (logger.isDebugEnabled()) {
            logger.debug("No XML 'id' specified - using '" + beanName +
                    "' as bean name and " + aliases + " as aliases");
        }
    }

    if (containingBean == null) {
        //检查bean名字，别名是不是已经使用过了
        checkNameUniqueness(beanName, aliases, ele);
    }
    // 待会我会介绍这个BeanDefinition的体系 这个方法到底干了啥？
    // 1.这个方法会解析bean 的class标签，parent的标签。
    // 2.然后会new一个GenericBeanDefinition，然后将class，parent的值，以及classload设置进去。
    // 3.解析标签下的meta，key，value标签，把依赖的关系也设置进去。
    AbstractBeanDefinition beanDefinition = parseBeanDefinitionElement(ele, beanName, containingBean);
    if (beanDefinition != null) {
        // 如果bean标签没有设置id，和name属性。
        if (!StringUtils.hasText(beanName)) {
            try {
                if (containingBean != null) {
                    beanName = BeanDefinitionReaderUtils.generateBeanName(
                            beanDefinition, this.readerContext.getRegistry(), true);
                }
                else {
                    // 如果bean标签没有设置id，和name属性。
                    // 自行创建一个名字。这里会调用BeanDefinitionReaderUtils.generateBeanName方法
                    beanName = this.readerContext.generateBeanName(beanDefinition);
                    // Register an alias for the plain bean class name, if still possible,
                    // if the generator returned the class name plus a suffix.
                    // This is expected for Spring 1.2/2.0 backwards compatibility.
                    // 获取beanClassName，其实就是class属性的值。
                    String beanClassName = beanDefinition.getBeanClassName();
                    // 如果名字是以className开头且没有被使用过的，就加入到别名里。
                    if (beanClassName != null &&
                            beanName.startsWith(beanClassName) && beanName.length() > beanClassName.length() &&
                            !this.readerContext.getRegistry().isBeanNameInUse(beanClassName)) {
                        aliases.add(beanClassName);
                    }
                }
                if (logger.isDebugEnabled()) {
                    logger.debug("Neither XML 'id' nor 'name' specified - " +
                            "using generated bean name [" + beanName + "]");
                }
            }
            catch (Exception ex) {
                error(ex.getMessage(), ele);
                return null;
            }
        }

        String[] aliasesArray = StringUtils.toStringArray(aliases);
        // 创建BeanDefinitionHolder类
        return new BeanDefinitionHolder(beanDefinition, beanName, aliasesArray);
    }

    return null;
}
```
## BeanDefinition接口
### 继承图谱
![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1558347721946.png)

### parseBeanDefinitionElement

接着看AbstractBeanDefinition beanDefinition = parseBeanDefinitionElement(ele, beanName, containingBean);这句的具体实现：
```java
public AbstractBeanDefinition parseBeanDefinitionElement(
            Element ele, String beanName, BeanDefinition containingBean) {
    //把名字进行一次压栈         
    this.parseState.push(new BeanEntry(beanName));

    String className = null;
    // 获取class属性值
    if (ele.hasAttribute(CLASS_ATTRIBUTE)) {
        className = ele.getAttribute(CLASS_ATTRIBUTE).trim();
    }

    try {
        String parent = null;
        // 获取parent的属性值
        if (ele.hasAttribute(PARENT_ATTRIBUTE)) {
            parent = ele.getAttribute(PARENT_ATTRIBUTE);
        }
        // 调用BeanDefinitionReaderUtils.createBeanDefinition（）
        // 创建GenericBeanDefinition实例，设置className，parent。
        AbstractBeanDefinition bd = createBeanDefinition(className, parent);
        // 这个方法会解析singleton、scope、abstract、lazy-init、autowire、
        // dependency-check、depends-on、init-method、autowire-candidate、
        // primary、destroy-method、actory-method、factory-bean、constructor-arg
        // index、type、value-type、key-type、property、ref、value等标签
        // 设置到 GenericBeanDefinition的实例里面。
        parseBeanDefinitionAttributes(ele, beanName, containingBean, bd);
        // 设置描述。
        bd.setDescription(DomUtils.getChildElementValueByTagName(ele, DESCRIPTION_ELEMENT));
        // 解析元数据标签
        parseMetaElements(ele, bd);
        // 解析lookup-method标签
        parseLookupOverrideSubElements(ele, bd.getMethodOverrides());
        // 解析replace-method标签
        parseReplacedMethodSubElements(ele, bd.getMethodOverrides());
        // 解析构造方法
        parseConstructorArgElements(ele, bd);
        // 解析属性依赖
        parsePropertyElements(ele, bd);
        // 解析Qualifier标签
        parseQualifierElements(ele, bd);
        // 设置资源 
        bd.setResource(this.readerContext.getResource());
        bd.setSource(extractSource(ele));

        return bd;
    }
    catch (Exception ex) {
        ... 
    }
    finally {
        this.parseState.pop();
    }

    return null;
}
```
其实这里面就已经把bean的定义bean的依赖关系都设置好了。但是bean并没有被实例化。
### parseMetaElements
```java
public void parseMetaElements(Element ele, BeanMetadataAttributeAccessor attributeAccessor) {
    NodeList nl = ele.getChildNodes();
    for (int i = 0; i < nl.getLength(); i++) {
        Node node = nl.item(i);
        if (isCandidateElement(node) && nodeNameEquals(node, META_ELEMENT)) {
            Element metaElement = (Element) node;
            String key = metaElement.getAttribute(KEY_ATTRIBUTE);
            String value = metaElement.getAttribute(VALUE_ATTRIBUTE);
             //就是一个key, value的载体，无他
            BeanMetadataAttribute attribute = new BeanMetadataAttribute(key, value);
             //sourceExtractor默认是NullSourceExtractor，返回的是空
            attribute.setSource(extractSource(metaElement));
            attributeAccessor.addMetadataAttribute(attribute);
        }
    }
}
```
AbstractBeanDefinition继承自BeanMetadataAttributeAccessor类，底层使用了一个LinkedHashMap保存metadata。这个metadata具体是做什么暂时还不知道。我们实际应用中meta标签也很少见。
例子：
```java
<bean id="b" name="one, two" class="base.SimpleBean">
    <meta key="name" value="dsfesf"/>
</bean>
```
### parseLookupOverrideSubElements
```java
public void parseLookupOverrideSubElements(Element beanEle, MethodOverrides overrides) {
    NodeList nl = beanEle.getChildNodes();
    for (int i = 0; i < nl.getLength(); i++) {
        Node node = nl.item(i);
        if (isCandidateElement(node) && nodeNameEquals(node, LOOKUP_METHOD_ELEMENT)) {
            Element ele = (Element) node;
            String methodName = ele.getAttribute(NAME_ATTRIBUTE);
            String beanRef = ele.getAttribute(BEAN_ELEMENT);
            //以MethodOverride的方式，存放在set集合里面
            LookupOverride override = new LookupOverride(methodName, beanRef);
            override.setSource(extractSource(ele));
            overrides.addOverride(override);
        }
    }
}
```
此标签的作用在于当一个bean的某个方法被设置为lookup-method后，每次调用此方法时，都会返回一个新的指定bean的对象。例如：
```java
<bean id="apple" class="a.b.c.Apple" scope="prototype"/>
<!--水果盘-->
<bean id="fruitPlate" class="a.b.c.FruitPlate">
    <lookup-method name="getFruit" bean="apple"/>
</bean>
```

### parseReplacedMethodSubElements
```java
public void parseReplacedMethodSubElements(Element beanEle, MethodOverrides overrides) {
    NodeList nl = beanEle.getChildNodes();
    for (int i = 0; i < nl.getLength(); i++) {
        Node node = nl.item(i);
        if (isCandidateElement(node) && nodeNameEquals(node, REPLACED_METHOD_ELEMENT)) {
            Element replacedMethodEle = (Element) node;
            //获取name属性
            String name = replacedMethodEle.getAttribute(NAME_ATTRIBUTE);
            //获取replace-method属性
            String callback = replacedMethodEle.getAttribute(REPLACER_ATTRIBUTE);
            ReplaceOverride replaceOverride = new ReplaceOverride(name, callback);
            // Look for arg-type match elements.
            // 获取所有的 arg-type的标签
            // 遍历所有节点，找到匹配的。以ReplaceOverride结构存储到list里面
            List<Element> argTypeEles = DomUtils.getChildElementsByTagName(replacedMethodEle, ARG_TYPE_ELEMENT);
            for (Element argTypeEle : argTypeEles) {
                String match = argTypeEle.getAttribute(ARG_TYPE_MATCH_ATTRIBUTE);
                match = (StringUtils.hasText(match) ? match : DomUtils.getTextValue(argTypeEle));
                if (StringUtils.hasText(match)) {
                    replaceOverride.addTypeIdentifier(match);
                }
            }
            replaceOverride.setSource(extractSource(replacedMethodEle));
            overrides.addOverride(replaceOverride);
        }
    }
}
```
replace-method 主要作用就是替换方法体及其返回值，使用比较简单。只需要实现MethodReplacer接口，并重写reimplement方法，然后就能完成方法的替换。这个有点类似aop的功能实现场景用的地方不是太多。
例子：
```java
<!-- ====================replace-method属性注入==================== -->
<bean id="dogReplaceMethod" class="com.lyc.cn.v2.day01.method.replaceMethod.ReplaceDog"/>
<bean id="originalDogReplaceMethod" class="com.lyc.cn.v2.day01.method.replaceMethod.OriginalDog">
    <replaced-method name="sayHello" replacer="dogReplaceMethod">
        <arg-type match="java.lang.String"></arg-type>
    </replaced-method>
</bean>
```
### parseConstructorArgElements
解析构造方法。构造方法注入
```
<bean class="base.SimpleBean">
    <constructor-arg>
        <value type="java.lang.String">Cat</value>
    </constructor-arg>
</bean>
```
```java
public void parseConstructorArgElements(Element beanEle, BeanDefinition bd) {
    NodeList nl = beanEle.getChildNodes();
    for (int i = 0; i < nl.getLength(); i++) {
        Node node = nl.item(i);
        if (isCandidateElement(node) && nodeNameEquals(node, CONSTRUCTOR_ARG_ELEMENT)) {

            parseConstructorArgElement((Element) node, bd);
        }
    }
}
```
```java
public void parseConstructorArgElement(Element ele, BeanDefinition bd) {
    String indexAttr = ele.getAttribute(INDEX_ATTRIBUTE);
    String typeAttr = ele.getAttribute(TYPE_ATTRIBUTE);
    String nameAttr = ele.getAttribute(NAME_ATTRIBUTE);
    //按照index的方式设置值
    if (StringUtils.hasLength(indexAttr)) {
        try {
            int index = Integer.parseInt(indexAttr);
            if (index < 0) {
                error("'index' cannot be lower than 0", ele);
            }
            else {
                try {
                    //ConstructorArgumentEntry其实存的就是index值
                    this.parseState.push(new ConstructorArgumentEntry(index));
                    //获取value标签的值                   
                    Object value = parsePropertyValue(ele, bd, null);
                    // ConstructorArgumentValues.valueHolder存储value值
                    //
                    ConstructorArgumentValues.ValueHolder valueHolder = new ConstructorArgumentValues.ValueHolder(value);
                    if (StringUtils.hasLength(typeAttr)) {
                        valueHolder.setType(typeAttr);
                    }
                    if (StringUtils.hasLength(nameAttr)) {
                        valueHolder.setName(nameAttr);
                    }
                    valueHolder.setSource(extractSource(ele));
                    //判断索引值是不是已经用过了。其实是检查的Map<index,ValueHolder>的key是否存在。
                    if (bd.getConstructorArgumentValues().hasIndexedArgumentValue(index)) {
                        error("Ambiguous constructor-arg entries for index " + index, ele);
                    }
                    else {
                        //把valueHolder加入到map里面
                        bd.getConstructorArgumentValues().addIndexedArgumentValue(index, valueHolder);
                    }
                }
                finally {
                    this.parseState.pop();
                }
            }
        }
        catch (NumberFormatException ex) {
            error("Attribute 'index' of tag 'constructor-arg' must be an integer", ele);
        }
    }
    //如果不是用index 就把名字相关信息加入ValueHOlde中存储到List里面。
    else {
        try {
            this.parseState.push(new ConstructorArgumentEntry());
            Object value = parsePropertyValue(ele, bd, null);
            ConstructorArgumentValues.ValueHolder valueHolder = new ConstructorArgumentValues.ValueHolder(value);
            if (StringUtils.hasLength(typeAttr)) {
                valueHolder.setType(typeAttr);
            }
            if (StringUtils.hasLength(nameAttr)) {
                valueHolder.setName(nameAttr);
            }
            valueHolder.setSource(extractSource(ele));
            bd.getConstructorArgumentValues().addGenericArgumentValue(valueHolder);
        }
        finally {
            this.parseState.pop();
        }
    }
}
```

# 配置加载
在 spring core 源码分析里面，我们阅读到了NamespaceHander这个接口。

spring 对配置的扩展都是通过这个NamspaceHandler来扩展的
在解析配置的时候都有调用parseCustomElement这个方法来解析扩展的配置。
```java
public BeanDefinition parseCustomElement(Element ele, BeanDefinition containingBd) {
    // 获取到命名空间值
    String namespaceUri = getNamespaceURI(ele);
    // 拿到名称空间处理器
    // 拿到解析上下文中NamespaceHandlerResolver的DefaultNamespaceHandlerResolver实例1.
    // 2.解析处理一下命名空间，拿到具体的NamespaceHandler
    NamespaceHandler handler = this.readerContext.getNamespaceHandlerResolver().resolve(namespaceUri);
    if (handler == null) {
        error("Unable to locate Spring NamespaceHandler for XML schema namespace [" + namespaceUri + "]", ele);
        return null;
    }
    return handler.parse(ele, new ParserContext(this.readerContext, this, containingBd));
}
```

this.readerContext.getNamespaceHandlerResolver().resolve(namespaceUri);
//最终会调用DefaultNamespaceHandlerResolver中的resolve方法：
```java
public NamespaceHandler resolve(String namespaceUri) {
        //这里就会加载所有的META-INF/spring.handlers
        //在事务模块这个文件的内容就是
        //http\://www.springframework.org/schema/tx=org.springframework.transaction.config.TxNamespaceHandler

        Map<String, Object> handlerMappings = getHandlerMappings();
        Object handlerOrClassName = handlerMappings.get(namespaceUri);
        if (handlerOrClassName == null) {
            return null;
        }
        //第一次解析这里肯定是不会进来的，因为handlerMappings加载的是字符串的key=value。
        else if (handlerOrClassName instanceof NamespaceHandler) {
            return (NamespaceHandler) handlerOrClassName;
        }
        else {
            //第一次会走这里，
            String className = (String) handlerOrClassName;
            try {
                //通过反射加载
                Class<?> handlerClass = ClassUtils.forName(className, this.classLoader);
                //校验一下是否是NamespaceHandler的子类
                if (!NamespaceHandler.class.isAssignableFrom(handlerClass)) {
                    throw new FatalBeanException("Class [" + className + "] for namespace [" + namespaceUri +
                            "] does not implement the [" + NamespaceHandler.class.getName() + "] interface");
                }
                //反射构建实例
                NamespaceHandler namespaceHandler = (NamespaceHandler) BeanUtils.instantiateClass(handlerClass);
                //调用NamespaceHandler.init方法。
                namespaceHandler.init();
                //再次缓存到handlerMappings,下次就可以直接使用了
                handlerMappings.put(namespaceUri, namespaceHandler);
                return namespaceHandler;
            }
            catch (ClassNotFoundException ex) {
                throw new FatalBeanException("NamespaceHandler class [" + className + "] for namespace [" +
                        namespaceUri + "] not found", ex);
            }
            catch (LinkageError err) {
                throw new FatalBeanException("Invalid NamespaceHandler class [" + className + "] for namespace [" +
                        namespaceUri + "]: problem with handler class file or dependent class", err);
            }
        }
    }
```
由此我们可以来看看事务的配置类处理器TxNamespaceHandler
```java
public class TxNamespaceHandler extends NamespaceHandlerSupport {

    static final String TRANSACTION_MANAGER_ATTRIBUTE = "transaction-manager";

    static final String DEFAULT_TRANSACTION_MANAGER_BEAN_NAME = "transactionManager";


    static String getTransactionManagerName(Element element) {
        return (element.hasAttribute(TRANSACTION_MANAGER_ATTRIBUTE) ?
                element.getAttribute(TRANSACTION_MANAGER_ATTRIBUTE) : DEFAULT_TRANSACTION_MANAGER_BEAN_NAME);
    }

    //NamespaceHandler.init方法会被调用，

    @Override
    public void init() {
        //注册事务的advice解析器
        registerBeanDefinitionParser("advice", new TxAdviceBeanDefinitionParser());
        //注册事务注解驱动的解析器
        registerBeanDefinitionParser("annotation-driven", new AnnotationDrivenBeanDefinitionParser());
        //注册JTA事务管理的解析器
        registerBeanDefinitionParser("jta-transaction-manager", new JtaTransactionManagerBeanDefinitionParser());
    }

}
```
首先会执行init方法，然后执行NamespaceHandler.parser解析方法，这个方法会去通过registerBeanDefinitionParser注册到map里的解析器中去找对应的解析器进行解析。
这里注册了三个解析器。
在xml文件里如果遇到了<tx:annotation-driven/> <tx:advice> 和<tx:jta-transaction-manager/> 分别取出对应的解析器解析配置。

## AnnotationDrivenBeanDefinitionParser
先看下AnnotationDrivenBeanDefinitionParser这个解析器，代码不是很长直接贴出来

```java
//实现了BeanDefinitionParser接口
class AnnotationDrivenBeanDefinitionParser implements BeanDefinitionParser {

    //这是调用的入口方法。
    @Override
    public BeanDefinition parse(Element element, ParserContext parserContext) {
        //向Spring容器注册TransactionalEventListener工厂，TransactionalEventListener是Spring4.2引入的新特性，允许我们自定义监听器监听事务的提交或其它动作。
        registerTransactionalEventListenerFactory(parserContext);
        //<tx:annotation-driven proxy-target-class="true" order="0" mode="aspectj"/>
        String mode = element.getAttribute("mode");
        //如果配置是aspectj方式
        //如果没有配置默认是走的代理方式
        if ("aspectj".equals(mode)) {
            // mode="aspectj"
            registerTransactionAspect(element, parserContext);
        }
        else {
            // mode="proxy"
            //调用的是内部类的方法。
            //这个方法是干嘛用的？
            AopAutoProxyConfigurer.configureAutoProxyCreator(element, parserContext);
        }
        return null;
    }


    private void registerTransactionAspect(Element element, ParserContext parserContext) {
        String txAspectBeanName = TransactionManagementConfigUtils.TRANSACTION_ASPECT_BEAN_NAME;
        String txAspectClassName = TransactionManagementConfigUtils.TRANSACTION_ASPECT_CLASS_NAME;
        if (!parserContext.getRegistry().containsBeanDefinition(txAspectBeanName)) {
            RootBeanDefinition def = new RootBeanDefinition();
            def.setBeanClassName(txAspectClassName);
            def.setFactoryMethodName("aspectOf");
            registerTransactionManager(element, def);
            parserContext.registerBeanComponent(new BeanComponentDefinition(def, txAspectBeanName));
        }
    }

    private static void registerTransactionManager(Element element, BeanDefinition def) {
        def.getPropertyValues().add("transactionManagerBeanName",
                TxNamespaceHandler.getTransactionManagerName(element));
    }

    private void registerTransactionalEventListenerFactory(ParserContext parserContext) {
        RootBeanDefinition def = new RootBeanDefinition();
        def.setBeanClass(TransactionalEventListenerFactory.class);
        parserContext.registerBeanComponent(new BeanComponentDefinition(def,
                TransactionManagementConfigUtils.TRANSACTIONAL_EVENT_LISTENER_FACTORY_BEAN_NAME));
    }


    /**
     * Inner class to just introduce an AOP framework dependency when actually in proxy mode.
     */
    private static class AopAutoProxyConfigurer {

        public static void configureAutoProxyCreator(Element element, ParserContext parserContext) {
            //先留下来后面在分析
            AopNamespaceUtils.registerAutoProxyCreatorIfNecessary(parserContext, element);

            String txAdvisorBeanName = TransactionManagementConfigUtils.TRANSACTION_ADVISOR_BEAN_NAME;
            
            if (!parserContext.getRegistry().containsBeanDefinition(txAdvisorBeanName)) {
                //element的值是：[tx:annotation-driven: null]此时拿到eleSource是null
                Object eleSource = parserContext.extractSource(element);

                // Create the TransactionAttributeSource definition.
                // 注入AnnotationTransactionAttributeSource bean信息
                RootBeanDefinition sourceDef = new RootBeanDefinition(
                        "org.springframework.transaction.annotation.AnnotationTransactionAttributeSource");
                sourceDef.setSource(eleSource);
                sourceDef.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                String sourceName = parserContext.getReaderContext().registerWithGeneratedName(sourceDef);

                // Create the TransactionInterceptor definition.
                // 注入事务拦截器TransactionInterceptor bean
                RootBeanDefinition interceptorDef = new RootBeanDefinition(TransactionInterceptor.class);
                interceptorDef.setSource(eleSource);
                interceptorDef.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                registerTransactionManager(element, interceptorDef);
                // 处理依赖注入信息
                interceptorDef.getPropertyValues().add("transactionAttributeSource", new RuntimeBeanReference(sourceName));
                String interceptorName = parserContext.getReaderContext().registerWithGeneratedName(interceptorDef);

                // Create the TransactionAttributeSourceAdvisor definition.
                RootBeanDefinition advisorDef = new RootBeanDefinition(BeanFactoryTransactionAttributeSourceAdvisor.class);
                advisorDef.setSource(eleSource);
                advisorDef.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                advisorDef.getPropertyValues().add("transactionAttributeSource", new RuntimeBeanReference(sourceName));
                advisorDef.getPropertyValues().add("adviceBeanName", interceptorName);
                if (element.hasAttribute("order")) {
                    advisorDef.getPropertyValues().add("order", element.getAttribute("order"));
                }
                parserContext.getRegistry().registerBeanDefinition(txAdvisorBeanName, advisorDef);

                CompositeComponentDefinition compositeDef = new CompositeComponentDefinition(element.getTagName(), eleSource);
                compositeDef.addNestedComponent(new BeanComponentDefinition(sourceDef, sourceName));
                compositeDef.addNestedComponent(new BeanComponentDefinition(interceptorDef, interceptorName));
                compositeDef.addNestedComponent(new BeanComponentDefinition(advisorDef, txAdvisorBeanName));
                parserContext.registerComponent(compositeDef);
            }
        }
    }
}
```
configureAutoProxyCreator 这个方法最终会给容器注入BeanFactoryTransactionAttributeSourceAdvisor->TransactionInterceptor->AnnotationTransactionAttributeSource 这样bean信息
同时还会自动注入一个InfrastructureAdvisorAutoProxyCreator 类。
这个也就是AopNamespaceUtils.registerAutoProxyCreatorIfNecessary(parserContext, element);这个方法里做的事情。

## InfrastructureAdvisorAutoProxyCreator 类
先看一下类的继承图。
![enter description here](https://www.github.com/liuyong520/pic/raw/master/小书匠/1560573080384.png)
清楚的发现，InfrastructureAdvisorAutoProxyCreator 实现了BeanPostProcessor 和BeanFactoryAware接口 以及BeanClassLoaderAware的接口

那么InfrastructureAdvisorAutoProxyCreator 在实例化的时候，会依次执行BeanClassLoaderAware.setBeanClassLoader方法，BeanFactoryAware.setBeanFactory方法，以及，实现了BeanPostProcessor中的postProcessBeforeInitialization和postProcessAfterInitialization方法。

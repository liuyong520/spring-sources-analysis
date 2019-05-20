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
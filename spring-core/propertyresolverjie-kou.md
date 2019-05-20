<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [PropertyResolver接口](#propertyresolver%E6%8E%A5%E5%8F%A3)
  - [继承图谱](#%E7%BB%A7%E6%89%BF%E5%9B%BE%E8%B0%B1)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
再看看doResolvePlaceholders这个方法
```java
private String doResolvePlaceholders(String text, PropertyPlaceholderHelper helper) {
    //调用的就是PropertyPlaceholderHelper的replacePlaceholders方法
    //replacePlaceholders这个方法就会把text中包含${value}的值给替换成properties中value的值：比如:text是foo=${foo},而foo在properties的值是bar 那么text被替换后就是foo=bar。
    //这个方法会通过接口回调的方式调用getPropertyAsRawString方法
    return helper.replacePlaceholders(text, new PropertyPlaceholderHelper.PlaceholderResolver() {
        @Override
        public String resolvePlaceholder(String placeholderName) {
            //这个方法就是从properties里面以字符串的方式读取数据
            return getPropertyAsRawString(placeholderName);
        }
    });
}
```
先看看PropertyPlaceholderHelper的构造方法：
```java
//构造方法其实很简单就是一系列的赋值。
public PropertyPlaceholderHelper(String placeholderPrefix, String placeholderSuffix,
            String valueSeparator, boolean ignoreUnresolvablePlaceholders) {

    Assert.notNull(placeholderPrefix, "'placeholderPrefix' must not be null");
    Assert.notNull(placeholderSuffix, "'placeholderSuffix' must not be null");
    //placeholderPrefix默认值是${
    this.placeholderPrefix = placeholderPrefix;
    //placeholderSuffix默认值是}
    this.placeholderSuffix = placeholderSuffix;
    //wellKnownSimplePrefixes是一个map，里面存放着“{”,"}"、“[“,"]"、"(",")"。三对键值对。
    //默认拿到的是{
    String simplePrefixForSuffix = wellKnownSimplePrefixes.get(this.placeholderSuffix);
    if (simplePrefixForSuffix != null && this.placeholderPrefix.endsWith(simplePrefixForSuffix)) {
        //simplePrefix被赋值成{
        this.simplePrefix = simplePrefixForSuffix;
    }
    else {
        this.simplePrefix = this.placeholderPrefix;
    }
    this.valueSeparator = valueSeparator;
    this.ignoreUnresolvablePlaceholders = ignoreUnresolvablePlaceholders;
}
```
继续看replacePlaceholders:
```java
public String replacePlaceholders(String value, PlaceholderResolver placeholderResolver) {
    Assert.notNull(value, "'value' must not be null");
    //这是一个递归的方法。替换${}包围的值。
    return parseStringValue(value, placeholderResolver, new HashSet<String>());
}
```
看看这个方法：这里简单的替换就不介绍了，主要看看多次的递归调用是如何实现的。
比如:foo=${b${hello}};
```java
protected String parseStringValue(
        String strVal, PlaceholderResolver placeholderResolver, Set<String> visitedPlaceholders) {

    StringBuilder result = new StringBuilder(strVal);

    int startIndex = strVal.indexOf(this.placeholderPrefix);
    while (startIndex != -1) {
        int endIndex = findPlaceholderEndIndex(result, startIndex);
        if (endIndex != -1) {
            //1.第一次拿到的是b${hello}
            //2.第二次拿到的是hello
            String placeholder = result.substring(startIndex + this.placeholderPrefix.length(), endIndex);
            String originalPlaceholder = placeholder;
            //不允许循环替换
            //1.将b${hello}放入set集合
            //2.将hello放入set集合
            if (!visitedPlaceholders.add(originalPlaceholder)) {
                throw new IllegalArgumentException(
                        "Circular placeholder reference '" + originalPlaceholder + "' in property definitions");
            }
            // Recursive invocation, parsing placeholders contained in the placeholder key.
            //1.用b${hello}去placeholde
            //
            placeholder = parseStringValue(placeholder, placeholderResolver, visitedPlaceholders);
            // Now obtain the value for the fully resolved key...
            //调用PlaceholderResolver接口里面的方法。
            //这里其实就是获取${key},key的属性的值了。
            String propVal = placeholderResolver.resolvePlaceholder(placeholder);
            //如果属性值是null且this.valueSeparator不为空
            if (propVal == null && this.valueSeparator != null) {
                //判断是否有:。
                //例如foo:foo1 此时foo1为foo的默认值。如果从配置里面获取不到foo的值就使用默认值。
                int separatorIndex = placeholder.indexOf(this.valueSeparator);
                if (separatorIndex != -1) {
                    //拿到:符号之前的字符串
                    String actualPlaceholder = placeholder.substring(0, separatorIndex);
                    String defaultValue = placeholder.substring(separatorIndex + this.valueSeparator.length());
                    //继续获取key的值
                    propVal = placeholderResolver.resolvePlaceholder(actualPlaceholder);
                    if (propVal == null) {
                        propVal = defaultValue;
                    }
                }
            }
            //值这一部分很有意思，这一部分会把拿到的值检测全部替换一次。如果值里面也有${code},
            if (propVal != null) {
                // Recursive invocation, parsing placeholders contained in the
                // previously resolved placeholder value.
                //获取值的
                propVal = parseStringValue(propVal, placeholderResolver, visitedPlaceholders);
                //递归调用基本都是执行最里层的调用，然后一层一层的回归。替换最里层的字符串。
                result.replace(startIndex, endIndex + this.placeholderSuffix.length(), propVal);
                if (logger.isTraceEnabled()) {
                    logger.trace("Resolved placeholder '" + placeholder + "'");
                }
                //重新获取$的位置。
                startIndex = result.indexOf(this.placeholderPrefix, startIndex + propVal.length());
            }
            //这里是那些没有被替换的值的处理
            else if (this.ignoreUnresolvablePlaceholders) {
                // Proceed with unprocessed value.
                startIndex = result.indexOf(this.placeholderPrefix, endIndex + this.placeholderSuffix.length());
            }
            else {
                throw new IllegalArgumentException("Could not resolve placeholder '" +
                        placeholder + "'" + " in string value \"" + strVal + "\"");
            }
            visitedPlaceholders.remove(originalPlaceholder);
        }
        else {
            startIndex = -1;
        }
    }

    return result.toString();
}
```
这一块可以举个例子:大家就清楚了
```java
public void testRecurseInProperty() {
    String text = "foo=${bar}";
    final Properties props = new Properties();
    props.setProperty("bar", "${baz}");
    props.setProperty("baz", "bar");
    PropertyPlaceholderHelper helper = new PropertyPlaceholderHelper("${", "}");
    assertEquals("foo=bar",helper.replacePlaceholders(text, new PropertyPlaceholderHelper.PlaceholderResolver() {
        @Override
        public String resolvePlaceholder(String placeholderName) {
            return props.getProperty(placeholderName);
        }
    }));
}
```
再看一个例子：
```java
final PropertyPlaceholderHelper helper = new PropertyPlaceholderHelper("${", "}");

public void testRecurseInPlaceholder() {
        String text = "foo=${b${inner}}";
        Properties props = new Properties();
        props.setProperty("bar", "bar");
        props.setProperty("inner", "ar");

        assertEquals("foo=bar", this.helper.replacePlaceholders(text, props));

        text = "${top}";
        props = new Properties();
        props.setProperty("top", "${child}+${child}");
        props.setProperty("child", "${${differentiator}.grandchild}");
        props.setProperty("differentiator", "first");
        props.setProperty("first.grandchild", "actualValue");
        //这里是replacePlaceholders的另外的一个方法。
        assertEquals("actualValue+actualValue", this.helper.replacePlaceholders(text, props));
    }
```
介绍到这里就知道我们spring对于我们配置中${code}是如何处理的。但是我们的xml的配置压根就没有解析，仅仅只是对jvm环境变量参数，以及系统环境参数的一个字符替换而已。例如：
```java
System.setProperty("spring", "classpath");
ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("${spring}:config.xml");
SimpleBean bean = context.getBean(SimpleBean.class);
```
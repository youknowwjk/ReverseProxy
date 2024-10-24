package com.autel.cloud.pile.base.infrastructure.util;

import java.lang.annotation.Annotation;
import java.util.Map;
 
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;
/***
 * 
 * <p>Title: MyApplicationContext</p>  
 * <p>Description: ApplicationContextAware 通过它Spring容器会自动把上下文环境对象调用ApplicationContextAware接口中的setApplicationContext方法。
 * 				         通过这个上下文环境对象得到Spring容器中的Bean
 * 				         在我们写的工具类获取线程中不能直接通过Spring注入，这个时候就需要通过ApplicationContext获取Bean
 * </p>  
 * @author shy  
 */
@Service
public class MyApplicationContext implements ApplicationContextAware {
 
	private static ApplicationContext context;
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		context = applicationContext;
	}
	
	public static <T> T getBean(final Class<T> requiredType) {
		return context.getBean(requiredType);
	}
 
	public static <T> T getBean(final String beanName) {
		@SuppressWarnings("unchecked")
		final T bean = (T) context.getBean(beanName);
		return bean;
	}
 
	public static <T> Map<String, T> getBeans(final Class<T> requiredType) {
		return context.getBeansOfType(requiredType);
	}
 
	public static Map<String, Object> getBeansWithAnnotation(final Class<? extends Annotation> annotationType) {
		return context.getBeansWithAnnotation(annotationType);
	}
}
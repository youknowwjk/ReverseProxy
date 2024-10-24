package com.autel.cloud.pile.base.infrastructure.exception;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;

/**
 * @version 0.0.1-SNAPSHOT
 * @Class: MessageSourceUtil.java
 * @Description: 国际化内容获取工具类
 * @Author A19011 欧志黎
 * @Date 2020年10月16日
 */
@Component
public class MessageSourceUtil {
    private final static org.slf4j.Logger LOGGER = LoggerFactory.getLogger(MessageSourceUtil.class);

    @Autowired
    private MessageSource messageSource;

    public  String getMessage(String messageKey) {
        try{
            return messageSource.getMessage(messageKey, null, LocaleContextHolder.getLocale());
        }catch(Exception e){
            LOGGER.error("解析资源文件异常：{}", e.getMessage());
        }
        return "";
    }

    public  String getMessage(String messageKey,Object[] params) {

        try{
            if(params == null || params.length==0){
                return getMessage(messageKey);
            }
            String pattern = messageSource.getMessage(messageKey, null, LocaleContextHolder.getLocale());
            MessageFormat mf = new MessageFormat(pattern);
            String content = mf.format(params);
            return content;
        }catch(Exception e){
            LOGGER.error("解析资源文件异常：{}", e.getMessage());
        }
        return "";
    }
}
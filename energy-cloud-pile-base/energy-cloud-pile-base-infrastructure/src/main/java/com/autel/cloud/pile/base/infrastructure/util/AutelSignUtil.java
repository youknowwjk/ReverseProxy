package com.autel.cloud.pile.base.infrastructure.util;

import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * @Author A22282
 * @Date 2022/5/12 14:48
 */
@Component
@Slf4j
public class AutelSignUtil {

    @Value("${pay.autelKey}")
    private String autelKey;

    private static String autelKeys;

    @PostConstruct
    public void getAutelKeys() {
        autelKeys = this.autelKey;
    }

    /**
     * 将加密后的字节数组转换成字符串
     *
     * @param b 字节数组
     * @return 字符串
     */
    private static String byteArrayToHexString(byte[] b) {
        StringBuilder hs = new StringBuilder();
        String stmp;
        for (int n = 0; b != null && n < b.length; n++) {
            stmp = Integer.toHexString(b[n] & 0XFF);
            if (stmp.length() == 1) {
                hs.append('0');
            }
            hs.append(stmp);
        }
        return hs.toString().toLowerCase();
    }

    /**
     * sha256_HMAC加密
     *
     * @param message 消息
     * @param secret  秘钥
     * @return 加密后字符串
     */
    private static String sha256_HMAC(String message, String secret) throws NoSuchAlgorithmException, InvalidKeyException {
        String hash = "";
        Mac sha256_HMAC = Mac.getInstance("HmacSHA256");
        SecretKeySpec secret_key = new SecretKeySpec(secret.getBytes(), "HmacSHA256");
        sha256_HMAC.init(secret_key);
        byte[] bytes = sha256_HMAC.doFinal(message.getBytes());
        hash = byteArrayToHexString(bytes);
        return hash;
    }

    /**
     * 按照红黑树（Red-Black tree）的 NavigableMap 实现
     * 按照字母大小排序
     */
    private static Map<String, Object> sort(Map<String, Object> map) {
        if (map == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> result = new TreeMap<>((Comparator<String>) String::compareTo);
        result.putAll(map);
        return result;
    }

    /**
     * 组合参数
     *
     * @param map
     * @return 如：key1Value1Key2Value2....
     */

    private static String groupStringParam(Map<String, Object> map, String key) {
        if (map == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, Object> item : map.entrySet()) {
            if (item.getValue() != null) {
                sb.append(item.getKey()).append("=");
                if (item.getValue() instanceof List) {
                    sb.append(JSON.toJSONString(item.getValue()));
                } else {
                    sb.append(item.getValue());
                }
                sb.append("&");
            }
        }
        sb.append("key").append("=").append(key);
        log.info("计算签名，源字段：\n{}", sb.toString());
        return sb.toString();
    }

    /**
     * bean转map
     *
     * @param obj
     * @return
     */
    public static Map<String, Object> bean2Map(Object obj) throws IntrospectionException, InvocationTargetException, IllegalAccessException {
        if (obj == null) {
            return Collections.emptyMap();
        }
        if (obj instanceof Map) {
            Map<String, Object> objMap = (Map<String, Object>) obj;
            Map<String, Object> objMap2 = new HashMap<>();
            objMap.forEach((k, v) -> {
                if (v != null) {
                    objMap2.put(k, v);
                }
            });
            return objMap2;
        }
        Map<String, Object> map = new HashMap<>();
        BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
        PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
        for (PropertyDescriptor property : propertyDescriptors) {
            String key = property.getName();
            // 过滤class属性
            if (!key.equals("class")) {
                // 得到property对应的getter方法
                Method getter = property.getReadMethod();
                Object value = getter.invoke(obj);
                if (value == null) {
                    continue;
                }
                if (isJsonObject(value)) {
                    map.putAll(bean2Map(value));
                } else {
                    map.put(key, value);
                }
            }
        }
        return map;
    }

    public static boolean isJsonObject(Object content) {
        try {
            JSON.parseObject(JSON.toJSONString(content));
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static String sha256_HMAC(Map<String, Object> obj, String secret) throws InvalidKeyException, IntrospectionException, NoSuchAlgorithmException, InvocationTargetException, IllegalAccessException {
        return sha256_HMAC(groupStringParam(sort(obj), secret), secret);
    }

    /**
     * 计算方法签名
     *
     * @return
     */
    public static String getSign(Map<String, Object> message) {
        String secret = null;
        try {
            secret = AutelSignUtil.sha256_HMAC(message, autelKeys);
        } catch (Exception e) {
            log.error("error found ", e);
        }
        if (secret != null) {
            return secret.toUpperCase();
        }
        return null;
    }

    /**
     * 计算方法签名
     *
     * @return
     */
    public static String getSigns(Object header, Object body) {
        String secret = null;
        try {
            Map<String, Object> map = new HashMap<>();
            Map<String, Object> headerMap = AutelSignUtil.bean2Map(header);
            if (headerMap != null) {
                map.putAll(headerMap);
            }
            Map<String, Object> bodyMap = AutelSignUtil.bean2Map(body);
            if (bodyMap != null) {
                map.putAll(bodyMap);
            }
            secret = AutelSignUtil.sha256_HMAC(map, autelKeys);
        } catch (Exception e) {
            log.error("error found ", e);
        }
        if (secret != null) {
            return secret.toUpperCase();
        }
        return null;
    }

    /**
     * 计算方法签名
     *
     * @return
     */
    public static String getSign(Object header, Object body) {
        String secret = null;
        try {
            Map<String, Object> map = new HashMap<>();
            Map<String, Object> headerMap = AutelSignUtil.bean2Map(header);
            if (headerMap != null) {
                map.putAll(headerMap);
            }
            Map<String, Object> bodyMap = AutelSignUtil.bean2Map(body);
            if (bodyMap != null) {
                map.putAll(bodyMap);
            }
            secret = AutelSignUtil.sha256_HMAC(map, autelKeys);
        } catch (Exception e) {
            log.error("error found ", e);
        }
        if (secret != null) {
            return secret.toUpperCase();
        }
        return null;
    }
}

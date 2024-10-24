package com.autel.cloud.pile.base.domain.convert;

import com.google.common.collect.Lists;
import org.dozer.DozerBeanMapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class DozerConvert {

    private DozerConvert() {

    }

    /**
     * 持有Dozer单例, 避免重复创建DozerMapper消耗资源.
     */
    private static DozerBeanMapper dozer;

    static {
        if (dozer == null) {
            dozer = new DozerBeanMapper();
            List<String> mappingFileUrls = Lists.newArrayList("dozer-config.xml");
            dozer.setMappingFiles(mappingFileUrls);
        }
    }

    /**
     * @param source           源对象
     * @param destinationClass 目标对象
     * @return
     * @title: map
     * @description: 单个对象相互转换
     */
    public static <T> T map(Object source, Class<T> destinationClass) {
        return dozer.map(source, destinationClass);
    }

    /**
     * @param sourceList
     * @param destinationClass
     * @return
     * @title mapList
     * @description 集合对象相互转换
     */
    public static <T> List<T> mapList(Collection sourceList, Class<T> destinationClass) {
        List<T> destinationList = new ArrayList<T>();
        for (Object sourceObject : sourceList) {
            T destinationObject = dozer.map(sourceObject, destinationClass);
            destinationList.add(destinationObject);
        }
        return destinationList;
    }

    /**
     * 基于Dozer将对象A的值拷贝到对象B中
     *
     * @param source 需要转换的对象
     * @param toObj  转换后对象类型
     */
    public static void copy(Object source, Object toObj) {
        if (null != source) {
            dozer.map(source, toObj);
        }
    }
}


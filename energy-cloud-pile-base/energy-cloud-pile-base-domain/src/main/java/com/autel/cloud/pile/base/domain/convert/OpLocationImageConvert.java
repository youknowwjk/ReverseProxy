package com.autel.cloud.pile.base.domain.convert;


import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationImageEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class OpLocationImageConvert {

    private OpLocationImageConvert() {

    }

    /**
     * 转换
     *
     * @param opImageEntity
     * @param opLocationDTO
     * @return
     */
    public static OpLocationImageEntity buildOpLocationImageEntity(OpImageEntity opImageEntity, OpLocationDTO opLocationDTO) {
        OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
        opLocationImageEntity.setLocationId(opLocationDTO.getId());
        opLocationImageEntity.setImageId(opImageEntity.getId());
        opLocationImageEntity.setDeleted(0);
        opLocationImageEntity.setCreatedAt(new Date().getTime());
        opLocationImageEntity.setUpdatedAt(new Date().getTime());
        return opLocationImageEntity;
    }

    /**
     * 转换
     *
     * @param json
     * @return
     */
    public static List<OpImageEntity> toOpImageEntityList(JSONObject json) {
        List<OpImageEntity> imageEntityList = new ArrayList<>();
        if (StringUtils.isNotBlank(json.getString("imagePath"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("ImagePath")));
        }
        if (StringUtils.isNotBlank(json.getString("img1"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img1")));
        }
        if (StringUtils.isNotBlank(json.getString("img2"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img2")));
        }
        if (StringUtils.isNotBlank(json.getString("img3"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img3")));
        }
        if (StringUtils.isNotBlank(json.getString("img4"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img4")));
        }
        if (StringUtils.isNotBlank(json.getString("img5"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img5")));
        }
        if (StringUtils.isNotBlank(json.getString("img6"))) {
            imageEntityList.add(buildOpImageEntity(json.getString("img6")));
        }
        return imageEntityList;
    }

    /**
     * 转换
     *
     * @param url
     * @return
     */
    public static OpImageEntity buildOpImageEntity(String url) {
        OpImageEntity opImageEntity = new OpImageEntity();
        opImageEntity.setDeleted(0);
        opImageEntity.setStatus(0);
        opImageEntity.setType("jpg");
        opImageEntity.setCreatedAt(System.currentTimeMillis());
        opImageEntity.setUpdatedAt(System.currentTimeMillis());
        opImageEntity.setThumbnail(null);
        opImageEntity.setUrl(url);
        opImageEntity.setWidth(0);
        opImageEntity.setHeight(0);
        return opImageEntity;
    }
}

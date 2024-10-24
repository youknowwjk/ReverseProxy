package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.vo.OpEvseTypeVO;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;

import java.util.List;

/**
 * @ClassName OpEvseTypeService
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:14
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpEvseTypeService {

    /**
     *  枪类型列表
     * @param
     * @return
     */
    Result<List<OpEvseTypeVO>> list();

    /**
     *  枪类型列表 - APP
     * @param
     * @return
     */
    Result<List<GunTypeRespDTO>> getGunType();

    Result<Boolean> syncGunType();
}

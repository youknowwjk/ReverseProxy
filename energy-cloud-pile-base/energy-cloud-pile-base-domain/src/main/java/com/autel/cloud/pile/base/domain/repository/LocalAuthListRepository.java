package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.LocalListInformationDTO;
import com.autel.cloud.pile.base.dto.SaveAuthListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface LocalAuthListRepository extends IService<LocalAuthListEntity> {
    List<LocalAuthListEntity> getAllAuthListBySellerId(String sn,Long sellerId);

    Boolean saveAuthList(SaveAuthListDTO saveAuthListDTO);

    Boolean updateAuthList(LocalListInformationDTO localListInformationDTO);

    Boolean deletedAuthList(Long id,String sn);

    Boolean sendAllAuthList(SaveAuthListDTO saveAuthListDTO);
}

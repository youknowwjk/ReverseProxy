package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.LocalListInformationDTO;
import com.autel.cloud.pile.base.dto.SaveAuthListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.autel.cloud.pile.base.vo.GetAllAuthListVO;

import java.util.List;

public interface LocalAuthListService {
    List<LocalAuthListEntity> getAllAuthListBySellerId(String sn,Long sellerId);

    Boolean saveAuthList(SaveAuthListDTO saveAuthListDTO);

    LocalAuthListEntity getDetail(Long id);

    Boolean updateAuthList(LocalListInformationDTO localListInformationDTO);

    Boolean deletedAuthList(Long id,String sn);

    List<GetAllAuthListVO> getAllAuthList(String sn, Long sellerId);

    Boolean sendAllAuthList(SaveAuthListDTO saveAuthListDTO);
}

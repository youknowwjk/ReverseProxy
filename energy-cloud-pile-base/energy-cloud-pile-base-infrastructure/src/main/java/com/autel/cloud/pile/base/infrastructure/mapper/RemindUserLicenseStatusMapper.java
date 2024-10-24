package com.autel.cloud.pile.base.infrastructure.mapper;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.RemindUserLicenseStatusEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * 提示用户license状态 Mapper接口
 */
@Mapper
@Repository
public interface RemindUserLicenseStatusMapper extends BaseMapper<RemindUserLicenseStatusEntity> {
}

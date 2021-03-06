package org.im97mori.ble.android.peripheral.room;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

@Entity(tableName = "device_setting")
public class DeviceSetting {

    @PrimaryKey(autoGenerate = true)
    @ColumnInfo(name = "device_setting_id")
    private final long mId;

    @NonNull
    @ColumnInfo(name = "device_setting_name")
    private String mDeviceSettingName;

    @ColumnInfo(name = "device_type")
    private final int mDeviceType;

    @ColumnInfo(name = "device_setting_data")
    private byte[] mDeviceSettingData;

    @Ignore
    public DeviceSetting(long id) {
        this(id, "", 0, null);
    }

    @Ignore
    public DeviceSetting(int deviceType) {
        this(0, "", deviceType, null);
    }

    @Ignore
    public DeviceSetting(@NonNull String deviceSettingName, int deviceType) {
        this(0, deviceSettingName, deviceType, null);
    }

    public DeviceSetting(long id, @NonNull String deviceSettingName, int deviceType, @Nullable byte[] deviceSettingData) {
        mId = id;
        mDeviceSettingName = deviceSettingName;
        mDeviceType = deviceType;
        mDeviceSettingData = deviceSettingData;
    }

    public long getId() {
        return mId;
    }

    @NonNull
    public String getDeviceSettingName() {
        return mDeviceSettingName;
    }


    public void setDeviceSettingName(@NonNull String deviceSettingName) {
        this.mDeviceSettingName = deviceSettingName;
    }

    public int getDeviceType() {
        return mDeviceType;
    }

    @Nullable
    public byte[] getDeviceSettingData() {
        return mDeviceSettingData;
    }

    public void setDeviceSettingData(@Nullable byte[] deviceSettingData) {
        this.mDeviceSettingData = deviceSettingData;
    }

}

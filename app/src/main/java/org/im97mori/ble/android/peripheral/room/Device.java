package org.im97mori.ble.android.peripheral.room;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

@Entity(tableName = "devices")
public class Device {

    @PrimaryKey(autoGenerate = true)
    @ColumnInfo(name = "deviceid")
    private final long mId;

    @ColumnInfo(name = "devicesettingname")
    private String mDeviceSettingName;

    @ColumnInfo(name = "devicetype")
    private final int mDeviceType;

    @ColumnInfo(name = "devicesetting")
    private String mDeviceSetting;

    @Ignore
    public Device(long id) {
        this(id, "", 0, null);
    }

    @Ignore
    public Device(@NonNull String deviceSettingName, int deviceType) {
        this(0, deviceSettingName, deviceType, null);
    }

    public Device(long id, @NonNull String deviceSettingName, int deviceType, @Nullable String deviceSetting) {
        mId = id;
        mDeviceSettingName = deviceSettingName;
        mDeviceType = deviceType;
        mDeviceSetting = deviceSetting;
    }

    public long getId() {
        return mId;
    }

    @NonNull
    public String getDeviceSettingName() {
        return mDeviceSettingName;
    }


    public void setDeviceSettingName(@Nullable String deviceSettingName) {
        this.mDeviceSettingName = deviceSettingName;
    }

    public int getDeviceType() {
        return mDeviceType;
    }

    @Nullable
    public String getDeviceSetting() {
        return mDeviceSetting;
    }

    public void setDeviceSetting(@Nullable String deviceSetting) {
        this.mDeviceSetting = deviceSetting;
    }

}

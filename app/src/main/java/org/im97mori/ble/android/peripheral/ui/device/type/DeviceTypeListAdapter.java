package org.im97mori.ble.android.peripheral.ui.device.type;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.R;

import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DeviceTypeListAdapter extends ArrayAdapter<Pair<Integer, String>> {

    private final Map<Integer, Integer> mDeviceTypeImageResMap;

    private final LayoutInflater mLayoutInflater;

    public DeviceTypeListAdapter(@NonNull Context context
            , @NonNull Map<Integer, Integer> deviceTypeImageResMap
            , @NonNull List<Pair<Integer, String>> deviceTypeList) {
        super(context, 0, deviceTypeList);
        mDeviceTypeImageResMap = deviceTypeImageResMap;
        mLayoutInflater = LayoutInflater.from(context);
    }

    @Override
    public long getItemId(int position) {
        return super.getItemId(position);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        Pair<Integer, String> pair = getItem(position);
        if (convertView == null) {
            convertView = mLayoutInflater.inflate(R.layout.list_item, parent, false);
        }
        AppCompatTextView textView = (AppCompatTextView) convertView;
        textView.setText(pair.second);
        textView.setCompoundDrawablesRelativeWithIntrinsicBounds(Objects.requireNonNull(mDeviceTypeImageResMap.get(pair.first)), 0, 0, 0);
        return convertView;
    }
}
